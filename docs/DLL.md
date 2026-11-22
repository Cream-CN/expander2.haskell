# expander2 DLL（Windows x64）

在 CLI（`expander2-hs.exe`）之外，把同一份计算层直接做成 C ABI 的 DLL，供 MFC / C / C# /
其他语言直接调用。**不重新实现任何算法**：每个入口都只是 `Registry.byId` →
`Notation.expandNotation`，与 CLI 走同一条代码路，输出逐字节一致。

## 1. 两个产物

| 产物 | 目录 | 优化档 | 代码生成后端 |
|---|---|---|---|
| **正常版** `expander2.dll` | `.build-dll\` | `extreme`（`ghc-flags.mk` 的 `PROFILE_EXTREME`） | **LLVM**（`-fllvm` + `-optlo=-O3 -optlc=-O3`） |
| **兼容版** `expander2-compat.dll` | `.build-dll-compat\` | `compat`（`-O2`） | **GHC 内置 NCG**（不传 `-fllvm`） |

两个 DLL 之外还有：

- `expander2.h`（纯 ASCII 抗 MSVC C4819；每个产物目录各一份，仓库里提交在 `dll\expander2.h`）
- `expander2.def`（导出表真源，`LIBRARY` 名随产物不同）
- `<产物>.dll.a` GHC 自动产出的导入库，`clang -lexpander2` 用它链接
- `test\dll_smoke.py`、`dll\example\expander2_example.c`

兼容版为什么要存在：LLVM 后端依赖 GHC 自带的 `llc`/`opt`（GHC 9.14 配 LLVM 20），
换机器、换 GHC 版本、或目标环境不允许额外工具链时，用兼容版（纯 GHC NCG、只要求能跑
GHC 本身）更稳。两者 ABI **完全相同**，只是代码生成质量与体积不同。

> `EXPANDER2_BUILD()` 返回 `"expander2.haskell/dll 1.0 backend=llvm"` 或 `... backend=ncg`，
> 运行时可直接确认拿到的是哪一档。

## 2. 导出命名：把记号名直接当函数名

规则（`src/OmegaY/Dll.hs` 的 `symbolOf`，生成器与运行时自省共用同一份实现）：

1. id 全部转大写；
2. 非 `[0-9A-Za-z]` 的字符一律换成 `_`；
3. 连续 `_` 折成一个；
4. 若结果以数字开头，前置一个 `_`。

| id | 导出符号（fs） | 导出符号（alter） |
|---|---|---|
| `pps` | `PPS` | `PPS_ALTER` |
| `bms-v1` | `BMS_V1` | `BMS_V1_ALTER` |
| `gms-gbms-omega-p` | `GMS_GBMS_OMEGA_P` | `GMS_GBMS_OMEGA_P_ALTER` |
| `-1y-(n+1)ss` | `_1Y_N_1_SS` | `_1Y_N_1_SS_ALTER` |
| `t--1y-(n+1)ss` | `T_1Y_N_1_SS` | `T_1Y_N_1_SS_ALTER` |
| `empty` | `EMPTY` | `EMPTY_ALTER` |

114 个记号 × 2 = 228 个记号入口，加 12 个 `EXPANDER2_*` 通用/自省入口，再加 RTS 的
`hs_init` / `hs_exit`，合计 **242** 个导出符号，清单见 `.build-dll\gen\symbols.txt`
（构建脚本用它与 `objdump -p` 的导出表做**严格集合比对**，缺一个或多一个都算失败）。

## 3. 调用契约

```c
#include "expander2.h"

int main(int argc, char ** argv) {
  hs_init(&argc, &argv);          /* 必须先调用 */

  char * out = PPS("1,2,3", 3);   /* mode = fs */
  if (out) { puts(out); EXPANDER2_FREE(out); }
  else     { char * e = EXPANDER2_LAST_ERROR(); puts(e); EXPANDER2_FREE(e); }

  hs_exit();                      /* 卸载 DLL 之前调用 */
  return 0;
}
```

- **一个记号两个入口**：`<SYM>` 等价 CLI 的 `<id> fs`，`<SYM>_ALTER` 等价 `<id> alter`。
- **签名按 kind 分三种**（`EXPANDER2_KIND()` 返回同一套编号）：

  | kind | 编号 | 声明 |
  |---|---|---|
  | seq（17 个） | 0 | `char * F(const char * input, int term);` |
  | text（89 个） | 1 | `char * F(const char * input, int term);` |
  | param（8 个） | 2 | `char * F(const char * input, int term, int param);` |

- `input` 是 UTF-8 表达式，与 CLI 从 stdin 读到的**同一段文本**（不再有 PowerShell
  管道那种尾部换行，某些 `m1ynss` 记号对尾换行敏感，这正是 DLL 比管道更省事的地方）。
  `NULL` 按空串处理。
- `term` 从 1 起算，`param` 范围见 `EXPANDER2_PARAM_MIN/MAX/DEFAULT`。
  param 记号把 `param` 传进三个参数版本；非 param 记号忽略传进来的 `param`（入口里
  统一补 0）。
- **返回值**：成功 = malloc 出来的 UTF-8 字符串，**不带行尾**；失败 = `NULL`。
- **所有权**：所有返回的 `char *` 都必须 `EXPANDER2_FREE()` 释放，**不要**自己 `free()`。
  `EXPANDER2_LAST_ERROR()` 也是返回 malloc 串（errno 风格：进程级槽位、每次读都给一份
  独立副本），同样用 `EXPANDER2_FREE()`。
- 错误槽位是进程级的、**不是**线程局部的；同一轮里先用完再让别的线程追加。
- **异常不外泄**：记号内部抛出的 Haskell 异常（`error`、负索引、`head []` 等）在
  DLL 层被 `try` 拦下，返回 `NULL`，错误槽为 `内部错误：…`；不会以 RTS
  uncaught exception 的形式终止宿主进程。回归用例见 §5。这类"内部错误"与
  "解析失败"共用 NULL + 错误槽通道，宿主按错误文本前缀区分。

### 3.1 通用入口与自省

```c
char * out = EXPANDER2_EXPAND("pps", EXPANDER2_MODE_FS, 3, 0, "1,2,3");
```

- `EXPANDER2_EXPAND(id, mode, term, param, input)`：`id` 是**记号原 id**（不是大写符号名），
  `mode` 取 `EXPANDER2_MODE_FS(0)` / `EXPANDER2_MODE_ALTER(1)`。适合配置驱动、或按 id
  从 `--list` 里读出来的场合。
- `EXPANDER2_COUNT()` → 114；`EXPANDER2_ID/NAME/SYMBOL/KIND/PARAM_MIN/MAX/DEFAULT(i)`
  按注册表顺序（与 CLI `--list` 同一稳定排序）。`SYMBOL(i)` 就是第 2 节规则的产物，
  可以直接喂给 `GetProcAddress`。
- `EXPANDER2_BUILD()` → 版本 + 后端名。

## 4. 构建

```powershell
.\build-dll.ps1                  # 两个都建
.\build-dll.ps1 -Which normal    # 只建 LLVM 版（.build-dll\expander2.dll）
.\build-dll.ps1 -Which compat    # 只建 NCG 版（.build-dll-compat\expander2-compat.dll）
.\build-dll.ps1 -Example         # 建完顺手编译并运行 dll\example\expander2_example.c
.\build-dll.ps1 -Info            # 只打印将执行的命令
.\build-dll.ps1 -Clean           # 删掉 .build-dll*
```

- 优化档位与 GHC 位置**只**从 `ghc-flags.mk` 读（`DEFAULT_PROFILE`、`GHC_HOME`、
  `PROFILE_*`），与 `build.ps1` 同一份口径。
- 头文件与 wrapper 由 `tools\GenDll.hs` 生成：新增记号后重跑一次构建即可，生成器会重编
  （按 `src\**\*.hs` + `tools\GenDll.hs` 的时间戳判断）。
- 导出表由 `-optl-Wl,<abs>\expander2.def` 精确控制。**这一点是必须的**：GHC 的
  `-shared` 默认会把约 6 万个符号全导出去（含 RTS 内部 `ALLOC_BH_*`、`ghczminternal_*`）。
- 构建完会用 `objdump -p` 读回导出表，与 `symbols.txt` 做**严格集合比对**（缺一个或多一个
  都判失败）。`objdump` 优先取 PATH 里的；取不到就去 `%MSYS64%`、`D:\msys64\ucrt64\bin`、
  GHC 同目录这几处找。
- 参数变小时不会复用旧 obj（`.ghcflags` 参数戳比对，变了就清 `obj` 重编）。
- `-NoProbe` 跳过"参数是否被当前 GHC 接受"的预编译验证；`-NoThreaded` 去掉 `-threaded`。

### 4.0 内存不够时（本机实测踩到过）

`extreme` 档 + LLVM 编最重的模块（`XyV3`）时，单个 GHC 进程峰值能到 1.7 GB 量级；
并行 5 路很容易把机器打爆，典型报错：

```
ghc-9.14.1.exe: getMBlocks: VirtualAlloc MEM_COMMIT failed: 页面文件太小，无法完成操作
```

对策：`-Jobs 1` 串行编（内存紧张时唯一稳的档），并关掉无关程序。

另外 GHC 对 `-shared` **明确忽略** `-rtsopts` / `-with-rtsopts`
（会提示 `-rtsopts and -with-rtsopts have no effect with -shared`），所以本脚本不传这两个；
DLL 的 RTS 参数请由宿主用 `GHCRTS` 环境变量或 `hs_init_ghc` 设置。

### 4.1 为什么是 `-fllvm` 而不是 `-llvm`

GHC 9.14 的 LLVM 后端 flag 是 **`-fllvm`**。写成 `-llvm` 时 GHC 不认识，会原样转给链接器
变成 `-lvm`，最终报 `lld: error: unable to find library -llvm`——也就是说本仓库
`build.ps1` / `Makefile` / `CMakeLists.txt` 里原先写的 `-llvm` 从来没生效过
（`-Llvm auto` 只是探针通过后回退 NCG）。本次一并改成 `-fllvm`。

实证：`-fllvm` 编出的模块 `.o` 明显大于 NCG，且与 NCG 产物 MD5 不同；`-fllvm` + `-shared`
单阶段（一条 `ghc` 命令）即可完成编译与链接，不需要先 `--make -c` 再手工调 `llc`。
`llc`/`opt` 用 GHC 自带的那份（`...\ghc\9.14.1\mingw\bin`，LLVM 20.1.7）；
PATH 里若另有一套（例如 msys64 ucrt64 的 LLVM 23）不要显式指向它，版本不匹配会报 IR 错误。

## 5. 验证

```powershell
.\build-dll.ps1 -Which normal -Example

# 异常边界回归：抛出型输入必须返回 NULL + 错误槽，宿主进程不许被带崩
python test\dll_robustness.py --dll=.build-dll\expander2.dll        --expect-backend=llvm
python test\dll_robustness.py --dll=.build-dll-compat\expander2-compat.dll --expect-backend=ncg

# DLL 与 CLI 逐字节对拍（每记号 6 个语料输入 × fs/alter × term 1,2 × param 取值）
# 注意：两个变体要一次一个地跑，别并行——每个 id 起一个子进程加载 DLL，两档同时跑会撞
# 页面文件上限（WinError 1455 / RTS malloc failed: initEventsBuf）。
python test\dll_smoke.py --dll=.build-dll\expander2.dll        --bin=.build\expander2-hs.exe --expect-backend=llvm
python test\dll_smoke.py --dll=.build-dll-compat\expander2-compat.dll --bin=.build\expander2-hs.exe --expect-backend=ncg
```

对拍口径：DLL 返回值 vs CLI 的 stdout 去掉末尾那一个换行（Windows 上是 `\r\n`，GHC 的
`putStrLn` 走 native newline）；CLI 打出 `ERR:` 前缀的，比较 `ERR:` 之后的文本与 DLL 的
错误槽位。alter 模式对拍的是 `<SYM>_ALTER`，不是 `<SYM>`；param 类记号按
`min` / `max` / `default` 各取一次。原实现断言崩溃 / 不终止的输入（CLI 非零退出）自动跳过
——`dll_smoke.py` 一个 id 一个子进程跑，真碰上死循环也只损失那一个 id。

两个和编码有关的坑，`dll_smoke.py` 已处理，自己写对拍时要注意：

- CLI 的 stdout 编码看它启动时有没有控制台：有控制台时是 UTF-8，用
  `Start-Process -WindowStyle Hidden` 之类的无控制台方式拉起时会退到 ANSI（cp936），
  所以脚本先按 UTF-8 解，失败再按 cp936 解（`decode_cli`）。
- 子进程把结果 JSON 传回父进程时要 `PYTHONIOENCODING=utf-8`，否则含 `ω` 的输出会在
  父进程侧变成 `\ufffd`。

## 6. 使用限制（务必知会使用者）

1. **一个进程只加载一个变体**。`normal` 与 `compat` 两个 DLL 各自静态链了一份 RTS，
   同时 LoadLibrary 会撞 RTS 全局符号。要换档就换进程。
2. `hs_init` 之前不要调用任何入口；`hs_exit` 之后再调用是未定义行为。
3. DLL 依赖 **UCRT**（`api-ms-win-crt-*`），体积 14 MB 级（RTS 静态链入），不需要额外
   分发 GHC 运行时。
4. `-threaded` 默认开启（`-NoThreaded` 可关）。默认构建下的 RTS 只保证单线程调用安全；
   要跨线程并发调，请自行串行化入口调用（错误槽位是共享的，多线程并发下后写覆盖先写）。
5. 记号的语义边界（FS 截断、错误文本、quirk）与 CLI/原 C++ 实现**完全一致**，不做"改进"；
   与 CLI 的差异只有一处：DLL 不追加行尾换行，也不给 `ERR:` 前缀（错误走 NULL + 错误槽位）。
6. 某些记号（`m1ynss` 系列）对输入尾部的换行敏感；DLL 拿到的是调用方给的裸串，
   不要自己往里拼 `\n`。

## 7. 新增记号时

1. 在 `src/OmegaY/Fam/<Group>/Family.hs` 里加记号（按 `docs/CONTRACT.md`）。
2. 跑 `.\build-dll.ps1`：生成器会重编，`Expander2Exports.hs` / `expander2.h` /
   `symbols.txt` 自动更新；导出表校验会同步检查新符号。
3. 若新 id 的大写化跟已有符号撞车，生成器会在生成阶段直接报错（不会静默产出坏 DLL）。
