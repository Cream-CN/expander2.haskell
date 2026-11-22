# 给 expander2.haskell 套 UI：可行性与落地方案

> 本轮为**实测研究**，不是设计稿。所有结论都来自本机 `Release\` 下的现成产物
> （`expander2.dll` / `expander2-hs.exe` / `expander2-shell.exe`），日期 2026-11-22。

## 0. 结论先说

1. **不用动计算层就能套 UI。** `docs/DLL.md` 里的 C ABI 已经做到"数据驱动"：
   114 个记号可用 `EXPANDER2_COUNT / ID / NAME / KIND / PARAM_*` 枚举，用
   `EXPANDER2_EXPAND(id, mode, term, param, text)` 一个入口展开任意记号。
   UI 不需要为每个记号写代码，**一个窗体 + 一个下拉框就能覆盖全部 114 个**。
2. **最稳的接法是"进程外"**：UI 只调 `expander2-shell.exe run <id> --ui --json`。
   实测这条路径已经自带三件 UI 最需要的东西：原版 `MessageBoxW` 的校验文案、
   异常兜底（`内部异常: …`）、发散输入的 `--timeout` 中断。**零代码改动**。
3. **直连 DLL 只剩两个待补的洞**：界面级校验没有导出、没有超时。异常逃逸这类
   "点一下整个程序消失"的风险已在 DLL 层兜住（返回 `NULL` + `内部错误：…`）。
4. 本机工具链现成：.NET 11 SDK + WindowsDesktop 运行时、GHC 9.14.1、clang、python、node。
   推荐第一版用 **C# WinForms（.NET 11）做壳 + 子进程调用**，半天出可点的 PoC，
   2–3 天出可用版，第一版不需要动计算层。

---

## 1. 现状盘点：三个现成的接缝

| 接缝 | 产物 | 形态 | 适合谁 |
|---|---|---|---|
| CLI | `expander2-hs.exe` | `<id> <fs\|alter> <term> [param]`，表达式走 stdin | 对拍脚本、一次性调用 |
| 壳（CLI+REPL） | `expander2-shell.exe` | `list/show/run/batch/repl`，`--json`、`--ui`、`--timeout` | **UI 最省事的后端** |
| DLL | `expander2.dll`（38 MB，LLVM 档）/ `expander2-compat.dll`（25 MB，NCG 档） | 242 个导出符号，C ABI | 进程内直连、批量、性能 |

实测确认：

- `EXPANDER2_BUILD()` → `expander2.haskell/dll 1.0 backend=llvm`；
- `EXPANDER2_COUNT()` → **114**，与 `expander2-hs.exe --list` 的 114 行一致、
  `--missing` 为空；`coverage.json` 里 114 个记号全部 `bad=0`；
- 自省返回三类 kind（0=seq / 1=text / 2=param），与 `docs/DLL.md` 的 17/89/8 划分吻合；
- 单次展开耗时：典型 0.1–1.5 ms（`bms-v4`、`omega-y-medium` 实测），
  进程外调用一次 24–33 ms（exe 启动 + RTS 初始化）。

进程外调用实测 5 连发：`expander2-hs.exe` 119 ms / 5 次，`expander2-shell.exe` 167 ms / 5 次
—— 对"点按钮算一次"的交互完全够用；真正的开销在 RTS 启动，不在计算。

---

## 2. 实测：DLL 的自省能力够不够 UI 用

用 `ctypes` 直接加载 `Release\expander2.dll`，结果：

| UI 需要的元素 | DLL 是否提供 | 备注 |
|---|---|---|
| 记号列表 | ✅ `EXPANDER2_ID(i)` / `EXPANDER2_NAME(i)` | 顺序与 CLI `--list` 同一稳定排序（display name 首码位） |
| 记号分类（决定哪个输入框可用） | ✅ `EXPANDER2_KIND(i)` | 0 seq / 1 text / 2 param |
| 参数范围与默认值 | ✅ `PARAM_MIN/MAX/DEFAULT(i)` | 只有 8 个 param 类记号给值，其余返回 -1 |
| 通用展开 | ✅ `EXPANDER2_EXPAND(id, mode, term, param, input)` | 实测 `pps` fs→`1,2`、alter→`1,2,3`；`bms-v4` 正常 |
| 按记号直达的符号 | ✅ `<SYM>` / `<SYM>_ALTER` | 需要逐个 `[DllImport]` 时用，走 `GetProcAddress` 也行 |
| 错误文本 | ✅ `EXPANDER2_LAST_ERROR()` | 未知 id 实测返回 `未知记号：nope` |
| 异常会不会带崩宿主 | ✅ DLL 层已兜底 | 展开里的 `error` / 负索引被收成 `NULL` + `内部错误：…`（见 §3.1） |
| **原版界面级校验** | ❌ **没有导出** | `EXPANDER2_EXPAND(bms-v4, fs, term=0, …)` 实测直接返回 `(0,0)`，而不是原版的 `项数必须为正整数，\n你难道要…` |
| 标签 / tags | ❌ 只导出 id + name | CLI `list` 的"标签"列、`:tags` 拿不到，需要新导出或读壳 |
| 超时 / 取消 | ❌ 无 | 进程内调用无法中途 kill，发散输入会挂住调用线程 |

结论：**UI 的"列表 + 参数 + 展开"三件事 DLL 都齐了，缺的是"界面校验出口、超时"**。

---

## 3. DLL 的边界：异常已兜底，还剩两个洞

### 3.1 异常边界（已在 DLL 层处理）

`expandWith` 在 `try` 内部深求值：用 `length (forceStr s)` 把结果逼到"脊 + 每个字符"，
记号内部抛出的 `error` / 负索引不会穿出 `foreign export`，统一返回 `NULL` +
`内部错误：…`，宿主进程不受影响。

验证：`1-mn` + `(0,0)(1,1)` / `(0,0)(1,1)(2,2)` 均返回 `NULL` + `内部错误：Prelude.!!: negative index`；
新增的 `test/dll_robustness.py` 守这条边界（两个后端 `cases=5 fails=0`）；
`dll_smoke.py` 全量（114 记号 732 例，normal）`mismatch=0`、`broken_ids=0`；
compat 跑 192 例子集同样 0 差异。

> 错误文本前缀是 `内部错误：`（与解析失败共用"NULL + 错误槽"通道），宿主靠前缀区分。
> 若把展开直接写在 UI 线程里，一次慢展开仍会卡住窗口 —— 这是 §3.3 的问题。

### 3.2 界面级校验没有出口

原版是"先 UI 校验、后展开"，`OmegaY.Notation.Types.uiExpand` 已经 1:1 复刻了
三条分支和全部文案（`docs/UI_VALIDATION.md`），但 **DLL 没导出它**。

两条修法，选一：

- **A（推荐）加一个导出**：`EXPANDER2_UI_EXPAND(id, mode, termRaw, paramRaw, input)`，
  内部 `byId` → `uiExpand`。签名用 `const char *` 接收 term/param 原文，
  这样 `abc` / `12abc` / `-1` 的 `std::stoi` 语义能完整保留 —— 这是关键，
  传 `int` 就丢了"项数必须是整数"那条路径。
- **B 不加导出，UI 侧重实现**：把 `Types.hs` 的三分支抄到 UI 里，
  文案硬编码。可行（只有 5 句），但两份真相，将来改文案必漏。

### 3.3 没有超时

`EXPANDER2_EXPAND` 是同步阻塞调用。实测 `omega-y-medium` 输入 `0,0,0,0`
走壳 `--timeout 2` 时 2.02 s 返回 `展开超时（2 秒），已中断`；直连 DLL 则会把
调用线程钉死，UI 线程直接假死。

修法：加 `EXPANDER2_EXPAND_TIMEOUT(..., int timeoutSec)`，内部抄
`OmegaY.Shell.runExpand` 的 `timeout` 包法（注意它注释里那条坑：
`timeout` 必须包在求值动作外层，不能放在内层 `catch` 里）。

---

## 4. 四条路线对比

| 路线 | 做法 | 新增依赖 | 保真度 | 崩溃/发散风险 | 工作量 |
|---|---|---|---|---|---|
| **A 子进程壳**（推荐首版） | WinForms/WPF 调 `expander2-shell.exe run … --ui --json` | .NET 桌面运行时（已有） | 校验文案、错误文本、超时**全部与 CLI 一致** | 崩溃只死子进程，UI 存活 | PoC 半天 / 可用 2–3 天 |
| **B 直连 DLL** | WinForms + P/Invoke，自省填列表 | 无 | 展开一致，但缺 §3 的两项 | 异常已兜底（§3.1）；仍无超时，发散输入会挂线程 | 补洞 1–2 天 + UI 2 天 |
| **C 本地 Web** | Node/Python bridge（ctypes 调 DLL）+ 浏览器或 WebView2 | node/python 已有 | 同上 | 同 B（bridge 进程可被看门狗重启） | 3–5 天（前后端 + 打包） |
| **D Haskell 原生 GUI** | wxHaskell / gi-gtk 直接进 `app/` | 新包（且违反"只用 base/containers"约定） | 最高 | 同进程，异常与超时都要自己做兜底 | 依赖装不上的风险最高 |
| **E 只扩 TUI** | 现有 REPL 加行编辑/补全（haskeline） | 1 个包 | 够用但不"图形" | 壳已兜底 | 0.5 天 |

补充两条容易被忽略的选项：

- **复用原 MFC 界面**：`Expander.cpp` 里有完整的 UI 校验与 MessageBox 文案，
  如果手里的 MFC 工程能编（本机 **没有 MSBuild**，只有 dotnet/clang），
  把 `INotation` 后端换成 DLL 薄封装是**保真度最高**的路；但工程不在本仓库，
  且要先解决 MFC 工具链。
- **WASM**：GHC 9.14 有 wasm 后端，但 114 个记号 + 38 MB 级 RTS 打进浏览器，
  还要重做一套导出，收益（免安装）不如成本，暂不建议。

### 为什么推荐 A 而不是 B

不是因为 DLL 不好，而是**UI 最怕的是"点一下就没了"**。实测 A 路线已经拿到：

```json
{"ok":false,"err":"项数必须为正整数，\n你难道要尝试向内存反方向写入序列吗"}
{"ok":false,"err":"参数必须是 1 到 25 之间的整数"}
{"ok":false,"err":"内部异常: Prelude.!!: negative index"}
{"ok":false,"err":"展开超时（2 秒），已中断；可用 :timeout 调整或关闭"}
```

四句话正好对应 UI 的四种反馈：校验弹窗、参数越界、内部错误、超时。
B 路线要把这四句全补齐，等于把壳的能力在 DLL 里再做一遍。

---

## 5. 推荐方案：C# WinForms 外壳 + 子进程调用

### 5.1 窗体布局（对照原版语义，而不是对照原版像素）

```
┌────────────────────────────────────────────────────────────┐
│ 记号 [ ComboBox：114 项，显示 name，悬停显示 id ]  [搜索框]  │
├────────────────────────────────────────────────────────────┤
│ 表达式（多行 TextBox，等宽字体，Ctrl+Enter = 展开）          │
├────────────────────────────────────────────────────────────┤
│ 项数 [____]   参数 [____](param 类才启用)   模式 (•fs ○alter)│
│ [ 展开 ] [ FS_alter ] [ 清空 ] [ 复制结果 ] [ 存文件… ]      │
├────────────────────────────────────────────────────────────┤
│ 结果（多行 TextBox，只读，等宽，自动换行关闭，可 Ctrl+A/C）  │
├────────────────────────────────────────────────────────────┤
│ 状态栏：id · kind · 耗时 0.4 ms · 后端 shell                │
└────────────────────────────────────────────────────────────┘
```

要点：

- **项数/参数框按 kind 启停**：kind=2 才让参数框可用；kind=0 的"项数"语义是
  序列长度（`term<1` 会报错），kind=1 是矩阵展开步数。提示语可以直接用
  `show <id>` 的元数据（`caps` / 参数范围）。
- **错误分两级**：`ok:false` 且 `err` 是那 5 句校验文案之一 → 走**模态 MessageBox**
  （复刻原版观感，文案逐字含 `\n`）；其余 `err`（解析失败、内部异常、超时）
  → 走**结果区红色内联条**，不打断用户改输入。原版对"解析失败"也是弹窗，
  这里可以按 §11 的偏好切换。
- **不要自己写正则判项数**：一旦自己校验，就会跟 `std::stoi`
  语义（`12abc`→12）打架，应该全部交给 `--ui`。

### 5.2 调用骨架

```csharp
static readonly string Shell =
    Path.Combine(AppContext.BaseDirectory, "expander2-shell.exe");

static JsonNode Run(string id, string mode, string termRaw, string paramRaw,
                    string text, int timeoutSec = 0)
{
    var psi = new ProcessStartInfo(Shell)
    {
        UseShellExecute = false, CreateNoWindow = true,
        RedirectStandardInput = true, RedirectStandardOutput = true,
        StandardOutputEncoding = new UTF8Encoding(false),
        StandardInputEncoding  = new UTF8Encoding(false),
        WorkingDirectory = Path.GetDirectoryName(Shell)!
    };
    psi.ArgumentList.Add("run");
    psi.ArgumentList.Add(id);
    psi.ArgumentList.Add("-m");  psi.ArgumentList.Add(mode);
    psi.ArgumentList.Add("--ui");
    psi.ArgumentList.Add("--json");
    if (timeoutSec > 0) {
        psi.ArgumentList.Add("--timeout"); psi.ArgumentList.Add($"{timeoutSec}");
    }
    psi.Environment["EXPANDER2_NO_BANNER"] = "1";   // 省掉 stderr 的 6 行许可声明

    using var p = Process.Start(psi)!;
    p.StandardInput.Write(text);          // 原样写，不要自己拼 \r\n
    p.StandardInput.Close();
    var json = p.StandardOutput.ReadToEnd();
    return JsonNode.Parse(json)!;
}
```

注意：term / param 是 `-t` / `-p` 的**字符串原文**（形如 `-t abc`），
`--ui` 下按 `std::stoi` 解析，传 int 会丢掉"项数必须是整数"分支。

三个必须知道的坑：

1. **stdin 不要补换行**：`m1ynss` 系列对尾部换行敏感（`docs/DLL.md` §6.6）。
2. **stdout 只解析 JSON**：数据式调用只把 6 行短声明写 stderr，stdout 是纯 JSON；
   但为了省事仍建议设 `EXPANDER2_NO_BANNER=1`。
3. 未知 id / 用法错误会走退出码 1/2 并把错误写 stderr，UI 要读 `ExitCode`
   区分"展开失败"和"根本没跑起来"。

取消/超时：`--timeout N` 让壳自己中断（实测 2.02 s 返回）；用户点"取消"就直接
`p.Kill()` —— 子进程死掉不影响 UI，这是 A 路线最大的红利。

### 5.3 记号的元数据从哪来

- 首版最省事：`expander2-shell.exe list` 的表格（ID / 名称 / 标签 / 能力）直接解析；
  或用 `show <id>` 拿参数范围。缺点是解析文本表。
- **建议顺手加 `list --json`**（`src/OmegaY/Shell.hs` 里几行，不动算法），
  UI 就能拿到 `{id,name,tags,kind,param:{min,max,default,label},hasAlter,suffix}`。
- 或者：**只用 DLL 做自省**（`EXPANDER2_ID/NAME/KIND/PARAM_*` 是纯读注册表，
  不跑展开，实测安全），**展开仍然走子进程**。这个混合方案不用改壳就能一次拿全
  元数据；§3.1 修掉之后，剩下的顾虑只有 tags / hasAlter 拿不到。

注意 DLL 自省**不导出 tags**，也不导出 `hasAlter`/`suffix`；如果 UI 想显示
标签分组或"该记号无 alter 版本"的提示，补 `list --json` 更划算。

### 5.4 线程模型

- 一次只允许一个在跑的展开（按钮置灰 + 转圈），因为错误槽 / 子进程都是一次一份；
- 调用放后台线程，完成后 `Invoke` 回 UI 线程；
- 批量用 `batch --json` 一次进程喂整个文件，别一行一个进程。

### 5.5 打包

`Release\` 里已经有 `expander2.dll` / `expander2-compat.dll` / `expander2-shell.exe`
/ `expander2-hs.exe` / `expander2.h` / `Release.tar.xz`。UI 只要能定位
`expander2-shell.exe`（同目录或配置项），单文件 dotnet 发布 + 这几个 exe 一起 zip 即可。
DLL 依赖 UCRT，Win10+ 自带；不需要装 GHC。

---

## 6. 如果一定要 Web

- bridge 用 **Python + ctypes**（本轮写的最小探针就是雏形）或 **Node + ffi**，
  起一个 `localhost:PORT` 的 `/expand` 接口，前端静态页；
- 好处：样式自由、可远程、可做批量面板；
- 代价：多一个进程 + 端口 + 打包，且要处理"bridge 崩了要自动重启"；
- 或者 **WebView2 套 WinForms**：拿 Web 的界面、走 A 路线的调用，兼顾两者，
  但引入 WebView2 运行时依赖。

只有在"要给别人远程用"或"要做富交互（逐步展开、图形化矩阵）"时，Web 才比 WinForms 划算。

---

## 7. 本轮用到的实测脚本

```python
# 最小自省 + 展开（ctypes）。注意 hs_init 的第二个参数是 char***
import ctypes, sys
d = ctypes.WinDLL(sys.argv[1])
d.hs_init.argtypes = [ctypes.POINTER(ctypes.c_int),
                      ctypes.POINTER(ctypes.POINTER(ctypes.c_char_p))]
argc = ctypes.c_int(1)
argv = (ctypes.c_char_p * 2)(b"probe", None)   # 必须留 NULL 哨兵，否则 RTS 可能越界读
p = ctypes.cast(argv, ctypes.POINTER(ctypes.c_char_p))
d.hs_init(ctypes.byref(argc), ctypes.byref(p))
```

（第一次按 `char**` 传会直接 access violation，`0x00000065626F7270` 就是被当指针的 "probe" 字符串本身。）

实测矩阵（供回归用）：

| 调用 | 结果 |
|---|---|
| `EXPANDER2_BUILD()` | `expander2.haskell/dll 1.0 backend=llvm` |
| `COUNT()` | 114 |
| `EXPAND(pps, fs, 3, 0, "1,2,3")` | `1,2` |
| `EXPAND(pps, alter, 3, 0, "1,2,3")` | `1,2,3` |
| `EXPAND(bms-v4, fs, 2, 0, "(0,0)(1,1)(2,2)(3,3)")` | `(0,0)(1,1)(2,2)(3,2)` |
| `EXPAND(bms-v4, fs, 0, 0, "(0,0)(1,1)")` | `(0,0)`（**没有**界面校验） |
| `EXPAND(nope, …)` | `未知记号：nope` |
| `EXPAND(1-mn, fs, 1, 0, "(0,0)(1,1)")` | `NULL` + `内部错误：Prelude.!!: negative index`（异常边界，见 §3.1） |
| `shell run bms-v4 -t 0 --ui --json` | `{"ok":false,"err":"项数必须为正整数，\n你难道要…"}` |
| `shell run bms-v4 -t abc --ui --json` | `{"ok":false,"err":"项数必须是整数，\n所以说你为什么要这样输入"}` |
| `shell run bt1ynss -t 2 -p 99 --ui --json` | `{"ok":false,"err":"参数必须是 1 到 25 之间的整数"}` |
| `shell run 1-mn -t 1 --ui --json` | `{"ok":false,"err":"内部异常: Prelude.!!: negative index"}` |
| `shell run omega-y-medium -t 1 --timeout 2 --json` | `{"ok":false,"err":"展开超时（2 秒），已中断"}` |

---

## 8. 实施步骤与工期

| 步 | 内容 | 产出 | 估时 |
|---|---|---|---|
| 0 | 定路线（本文 §4） | 一句话决策 | — |
| 1 | WinForms PoC：下拉 + 输入 + 展开 + 结果 | 能手点，覆盖 114 个 | 0.5 天 |
| 2 | 元数据齐全：`list --json` 或 DLL 自省 + 参数框启停 | 参数型 8 个可用 | 0.5 天 |
| 3 | 错误策略：校验文案弹窗 + 其余内联 + 超时/取消 | 与原版观感对齐 | 0.5 天 |
| 4 | 历史 / 文件 / 复制 / 批量（`batch --json`） | 日常可用 | 1 天 |
| 5 | 打包 + 图标 + 版本横幅 | 可分发 zip | 0.5 天 |
| 6（可选） | 补 §3 的两项（校验出口 / 超时）后切直连 DLL | 去掉子进程、调用降到 ~1 ms | 1–2 天 |

合计：**第一版 2–3 天**（不含可选第 6 步）。第 1 步就能验证全部可行性，
建议先只做第 1 步再决定后面。

---

## 9. 验收口径（别只看"能跑"）

1. **逐字节对齐**：同一 `(id, mode, term, param, input)`，UI 结果区的内容
   必须等于 `expander2-shell.exe run … --json` 的 `out` 字段；
   验收语料用 `test/diff_ui.py` 那套（口径见 `docs/UI_VALIDATION.md` §1–§3）。
2. **校验文案逐字**：`term=abc/0/-1`、参数越界、空输入三种情况，
   UI 弹出的文本必须与 `--ui` 输出**完全相同**（含 `\n` 与吐槽句）。
3. **不崩**：114 个记号逐个点一遍（尤其 `1-mn` 这类会抛内部错误的输入），
   要求"UI 存活 + 有错误提示"。
4. **不假死**：`omega-y-medium` + `0,0,0,0`，点一次到出提示 ≤ 3 秒，期间窗口可拖动。
5. **列表完整**：下拉项数 = `--list` 行数（当前 114），且 8 个 param 记号的参数框可用。

---

## 10. 风险与合规

- **GPLv3**：整个项目是 GPLv3。UI 若直接链接 DLL 分发，按 GPL 应同样以 GPLv3
  发布并提供对应源码；调子进程在法理上更松，但自家项目建议整体沿用 GPLv3。
- **一个进程只能加载一个 DLL 变体**（normal / compat 各静态链了一份 RTS），
  UI 若直连要固定选一个，别做"运行时切换"。
- **错误槽是进程级的、非线程局部**（`docs/DLL.md` §3），并发调用会互相覆盖。
- **DLL 的 RTS 参数**要用 `GHCRTS` / `hs_init_ghc` 设，`-shared` 会忽略 `-with-rtsopts`。
- 本机 `Release\` 的产物是现成的；重编用根目录的 `build-shell.ps1` / `build-dll.ps1`，
  内存紧张时按 `docs/DLL.md` §4.0 用 `-Jobs 1`。

---

## 11. 待确认

1. **保真优先还是好看优先？** 保真 → WinForms + 复刻原版布局（半天起）；
   好看 → WPF / WebView2（多 1–2 天）。
2. **错误展示**：全部弹 MessageBox（像原版），还是"校验弹窗 + 解析失败内联"？
3. **§3.2 的界面校验出口要不要加？**（`EXPANDER2_UI_EXPAND`，term/param 收原文）。
   不加就得在 UI 里复刻 5 句文案，两份真相容易漂。
