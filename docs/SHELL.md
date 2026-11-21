# 交互式 + CLI 壳子（expander2-shell）

`app/Shell.hs` + `src/OmegaY/Shell.hs` 在计算层之上提供两层入口：

- **CLI**：`list` / `show` / `run` / `batch`，可脚本化，输出与 `app/Main.hs` 同格式；
- **REPL**：交互式设置记号、模式、项数、参数，然后直接敲表达式。

它**不替换** `app/Main.hs`。`test/` 下的对拍脚本（`diff*.py`、`smoke.py`、`filter_legal.py`）
仍走旧入口，`--ui` 语义也未改动；壳子只负责输入输出、状态管理，以及把展开过程
包进异常/超时/截断保护里。

## 构建与运行

```sh
./build-shell.ps1                  # Windows：-> .build-shell\expander2-shell.exe
make shell                         # Unix：-> .build-shell/expander2-shell
```

只依赖 GHC 自带包（base / containers / text / array / process / time）。

```sh
./.build-shell/expander2-shell.exe --version
./.build-shell/expander2-shell.exe repl
```

## CLI

| 命令 | 说明 |
|---|---|
| `list [过滤词]` | 列出记号，默认输出按显示宽度对齐的表格（ID / 名称 / 标签 / 能力），过滤词匹配 id / 名称 / 标签，另有 `--grep` `--tag`；`--plain` 回到旧的 `id<TAB>显示名` |
| `show <id>` | 打印元数据（名称、标签、caps、参数范围、是否有独立 alter、后缀）。支持**唯一前缀** |
| `run <id> [表达式]` | 展开一段表达式；省略表达式则从 stdin 读整块 |
| `batch <id> [文件]` | 逐行展开文件（省略则读 stdin），输出 `行号<TAB>输入<TAB>结果` |
| `repl` | 进入交互模式 |

选项：

| 选项 | 含义 |
|---|---|
| `-m/--mode fs\|alter` | 展开模式，默认 `fs` |
| `-t/--term N` | 项数，默认 `1`；`--ui` 下按 `std::stoi` 语义解析原始文本 |
| `-p/--param N` | 参数，默认 `0` |
| `--ui` | 走界面级校验（`Expander.cpp` 的判定顺序与提示语，含多行文案） |
| `--timeout N` | 单次展开超时秒数，`0` 表示不限（默认） |
| `--json` | `run` 输出单条 JSON，`batch` 输出 JSON 数组（含 `line`） |
| `--plain` | `list` 输出旧版 `id<TAB>显示名`（默认是对齐表格） |
| `--verbatim` | 保留输入结尾的 `\r\n`；默认裁掉 |
| `--fail-fast` | `batch` 遇到第一个错误即停止 |
| `--strict` | 展开失败时退出码 `1` |
| `-q/--quiet` | 成功时不打印结果 |

输出与退出码约定（与 `app/Main.hs` 对齐）：

- 成功：结果一行；
- 展开失败：`ERR:` + 转义后的错误文本（换行 → 字面 `\n`，反斜杠 → `\\`），**退出码 0**；
- 未知记号：`ERR_NO_NOTATION`，退出码 `1`；
- 用法错误：stderr 打印错误并附用法，退出码 `2`；
- 加 `--strict` 时展开失败退出码 `1`，便于脚本判断。

示例：

```sh
./.build-shell/expander2-shell.exe run pps "1,2,3" -t 3
./.build-shell/expander2-shell.exe run bms-v4 "(0,0,1)(1,1,1)" --ui -t abc
./.build-shell/expander2-shell.exe batch mrss121 test/corpus/mrss.txt -t 1 --json
```

## 交互模式

启动后先打印当前设置，提示符形如 `[pps fs t=3 p=0]>`。直接输入表达式即展开；
括号未闭合会自动续行（`… ` 提示），因此多行矩阵可以直接粘贴。

两个入口共用同一个 REPL：

- `expander2-shell.exe repl`；
- **主程序 `expander2-hs.exe` 不带任何参数（或 `-i` / `--interactive`）时也直接进入同一个交互会话**，
  不再只打印用法退出。给对拍脚本用时仍然走 `<id> <fs|alter> <term>` 的参数式调用，行为不变。

| 命令 | 说明 |
|---|---|
| `:help` / `:h` / `:?` | 命令帮助 |
| `:list [过滤词]` | 列出记号，与 CLI `list` 同一张对齐表格（ID / 名称 / 标签 / 能力） |
| `:show [id\|前缀]` | 记号元数据，缺省为当前记号 |
| `:use <id\|前缀>` | 切换记号；唯一前缀可省略后半段（Tab 补全的替代） |
| `:mode fs\|alter` | 切换模式；无独立 alter 的记号会提示回退 |
| `:term <整数>` / `:param <整数>` | 设置项数 / 参数（保留原始文本） |
| `:ui on\|off` | 界面级校验开关 |
| `:timeout <秒\|off>` | 单次展开超时 |
| `:maxout <字符数\|off>` | 结果打印截断上限（`:save` 仍写全文） |
| `:x <表达式>` / `:expand` / `:e` | 用当前设置展开 |
| `:paste` | 多行粘贴，单独一行 `.` 结束 |
| `:read <文件>` | 整个文件（换行视作空格）当一条表达式 |
| `:each <文件>` | 文件每行按当前设置展开 |
| `:save <文件>` | 把上一次成功结果写入 UTF-8 文件 |
| `:last` | 上一次的输入、结果、耗时 |
| `:history [n]` | 最近 n 条（默认 10，带耗时） |
| `:log [n]` | 磁盘历史 `~/.expander2_history` |
| `:status` / `:tags` | 当前设置 / 标签统计 |
| `:reset` | 重置为 `pps / fs / term=1 / param=0` |
| `:quit` / `:q` / `Ctrl-D` | 退出 |

`Ctrl-C` 只取消当前输入行，不会退出；展开过程中的 `Ctrl-C` 记为「已中断」。

## 稳健性设计

1. **强制求值 + 异常归类**：`runExpand` 先 `evaluate (length out)` 把结果逼出来，
   再 `try` 归类异常——`UserInterrupt` → `已中断`，其余 → `内部异常: …`。
   纯计算里的 `error`、`head []`、栈溢出都不会让 REPL 退出。
2. **超时**：`timeout` 直接包住求值动作（不能放在内层 catch 里面，否则会吞掉
   `timeout` 自己的内部异常，只能看到 `<<timeout>>`）。超时后 REPL 继续可用。
   本仓库已知的发散输入（如 `omega-y-medium` 的 `0,0,0,0`）实测 2 秒内返回。
3. **编码**：Windows 控制台启动时尝试 `chcp 65001`，成功则 stdin/stdout/stderr 统一
   UTF-8；重定向/管道一律 UTF-8。文件读取按 UTF-8 且去 BOM。
4. **持久历史**：表达式写入 `~/.expander2_history`（`USERPROFILE` 优先，其次 `HOME`），
   写入失败静默忽略；`batch`/`--json` 不记录。
5. **输出上限**：`:maxout` 只截断屏幕显示，`:save` 仍写完整结果。

## 已做的冒烟验证（O0 构建）

| 用例 | 结果 |
|---|---|
| `--version` | `已注册 114 / 114 个记号` |
| `run pps "1,2,3" -t 3` / `-m alter` | `1,2` / `1,2,3`（与 `expandNotation` 的 FS `init` 语义一致） |
| `run bms-v4 "(0)(1)" -t abc --ui` | `ERR:项数必须是整数，\n所以说你为什么要这样输入`（文本型长文案，转义正确） |
| `run omega-y-medium "0,0,0,0" --timeout 2` | `ERR:展开超时（2 秒）…`，2.02 s 返回，REPL 未被拖死 |
| `batch pps … --json` | JSON 数组，`line` / `in` / `out` / `ok` 齐全 |
| REPL 多行 | `(0,0,1` + `)(1,1,1)` 自动续行后正常展开 |
| REPL `:ui on` + `:term abc` | 长文案按行缩进显示，REPL 继续 |

## 已知限制

- 没有行编辑 / Tab 补全：只依赖 base，不引入 `haskeline`。替代方案是 `:use` 唯一前缀
  与 `:list` 过滤；真要补全再单独加依赖。
- 自动续行以单个空格拼接各行（各矩阵解析器本就把空白视作分隔），历史里记录的是拼接后的文本。
- 非 tty（管道驱动）时提示符与结果会挤在同一行，这是提示符直接 `putStr` 的必然结果，
  交互终端下正常。
- 历史文件只记表达式，不记 `:` 命令；`:log` 读整个文件后取尾部。
- `--ui` / `:ui on` 只复刻界面层校验，不覆盖各记号内部的合法性规则（那仍由展开函数负责）。
- 超时依赖异步异常；若某个记号进入不分配内存的紧凑循环，中断可能被延迟。
