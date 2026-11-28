# 00 · 通用骨架（每个记号都要经过的部分）

本文只写「所有 119 个 id 共同遵守」的那一段流程，各族手册里不再重复；
族手册只写**该族进入实现函数之后**发生的事。

## 1. 一次调用的全过程

```
expander2-hs --id <id> [--term N] [--param N] [--file <path>]
```

| 阶段 | 位置 | 行为 |
|---|---|---|
| 取参 | `app/Main.hs:37`（`getArgs`）→ `collect`（`app/Main.hs:58`） | 只认 `--help` / `--list` / `--id` / `--term` / `--param` / `--file`；其它 `--x` 报「未知参数：x」，裸参数报「多余的位置参数：x」 |
| 默认值 | `app/Main.hs:24`（`opts0`） | `--term` 缺省 **0**，`--param` 缺省 **0** |
| 读表达式 | `app/Main.hs:47` | 有 `--file` 读文件，否则读 stdin（`getContents`，整段文本原样） |
| 查表 | `app/Main.hs:50` → `Registry.byId`（`src/OmegaY/Registry.hs:199`） | 查不到 → 打印 `ERR_NO_NOTATION` 并 `exitFailure` |
| 分派 | `app/Main.hs:53` → `Notation.Types.expandNotation`（`src/OmegaY/Notation/Types.hs:61`） | 按 `nCaps` 三型走不同校验，再调族实现 |
| 收尾 | `app/Main.hs:54-55` | `Left` → 打印 `ERR:<文本>`；`Right` → 打印结果串 |

判据顺序是「`byId` 先于 `Caps`」：id 写错时永远只看到 `ERR_NO_NOTATION`，看不到族内错误。

## 2. Caps 三型与分派（`Types.hs:61-80`）

| 型 | 构造者 | 实现签名 | `expandNotation` 做的事 |
|---|---|---|---|
| `TextCaps` | `textNotation` / `textAlterNotation` | `String -> Int -> String`（文本, term） | 直接 `expand text term ++ suffix`，**不做任何校验** |
| `ParamCaps` | `paramNotation` / `paramAlterNotation` | `String -> Int -> Int -> String`（文本, term, param） | 先查 `nParam` 的 `(mn,mx)` 区间，再 `expand text term param ++ suffix` |
| `SeqCaps` | `seqNotation` | `[Int] -> Int -> [Int]`（整数序列, term） | `term < 1` 先拦；再 `parseSequence text`，空则拦；最后 `seqToString (expand seq term) ++ suffix` |

`nParam = Just (min, max, default, label)`，四个字段分别用于：区间校验下界、上界、UI 预填值、输入框标签。

## 3. 逐字错误文本（照抄 C++ 侧，勿改写）

| 条件 | 文本 | 出处 |
|---|---|---|
| `ParamCaps` 且 `param < mn` 或 `param > mx` | `参数必须是 <mn> 到 <mx> 之间的整数` | `Types.hs:70-72` |
| `SeqCaps` 且 `term < 1` | `项数必须为正整数` | `Types.hs:75` |
| `SeqCaps` 且 `parseSequence text` 为空 | `序列不能为空` | `Types.hs:79` |
| id 未注册 | `ERR_NO_NOTATION`（无 `ERR:` 前缀） | `app/Main.hs:51` |

`expandNotation` 只产生上表前三条；族内部还可以自己返回 `Left`（各族手册各自列全）。

## 4. 三条必须记住的 quirk

1. **CLI 不校验 `term` 的正负**（`TextCaps` / `ParamCaps` 路径）。`--term 0` 或缺省 0 会原样传进实现函数，
   由实现自己决定 `max 1 n0` 之类的兜底。只有 `SeqCaps` 在 `Types.hs:75` 拦 `term < 1`。
2. **CLI 不校验空文本**（`TextCaps` / `ParamCaps` 路径）。空表达式照样进实现，往往展开成空串。
   界面层的三句提示语（`Types.hs:89-92`：`项数必须是整数，\n所以说你为什么要这样输入` 等）
   属于 `uiExpand`，CLI 不走这条路径。
3. **`ERR:` 不影响退出码**。`app/Main.hs:54` 只 `putStrLn ("ERR:" ++ err)`，不再 `exitFailure`；
   只有 `ERR_NO_NOTATION` 会非零退出。

## 5. 输入 / 输出文本格式

- **序列**：逗号分隔整数。`parseSequence`（`src/OmegaY/Parse.hs:16`）按 `,` 切，去掉空白后逐段 `stoi`
  （`Parse.hs:41`），解析不出的段**静默丢弃**；全丢弃则得 `[]` → 触发「序列不能为空」。
  输出侧 `seqToString`（`Parse.hs:65`）：空序列打印 `[]`，否则逗号连接。
- **矩阵**：`(a,b)(c,d)[n]`。`parseMatrix`（`Parse.hs:77`）第一遍 `maxRowCount` 统计括号内最大逗号数定行数，
  第二遍 `secondPass` 逐列落数、短列补 0；`[` 之后的十进制串即**默认 term**。
  quirk：负号被吞掉（C++ 原实现 `sign` 只赋值不用，`Parse.hs:101-102,127-129`），所以 `(-1,0)` 读成 `(1,0)`。
  输出侧 `matrixToString`（`Parse.hs:137`）：`term > 0` 才在尾部追加 `[term]`。
- **后缀**：所有族实现返回的串最后都拼 `suffix`（`nCaps` 里那一串，多数为 `""`；
  `empty` 是 `"  (尚在重构中)"`）。后缀**不受 term 影响**，只表示「这版还在重构」之类的状态。

## 6. 构造器决定「哪个函数是活的」

四个构造器里有**两个会把第一个展开函数参数丢掉**（`Types.hs:43-46`、`Types.hs:54-58`）：

| 构造器 | 注册进 `nCaps` 的函数 | 哪个是死代码 |
|---|---|---|
| `textNotation` | 第 1 个（`expand`） | 无 |
| `textAlterNotation` | **第 2 个（`alter`）** | 第 1 个（`_expand`，FS 版）被丢弃 |
| `paramNotation` | 第 1 个（`expand`） | 无 |
| `paramAlterNotation` | **第 2 个（`alter`）** | 第 1 个被丢弃 |

后果：凡是用 `textAlterNotation` / `paramAlterNotation` 注册的 id，手册的「入口函数」必须写**第二个**那支
（`expandAlter` / `expandNAlter` / `expandTextAlter` / `expandBt3Alter` …）；
写成 `expandText` / `expandFs` / `expandBt3` 就是错的——它们在注册表里根本没被接上，
是 C++ 侧 `FS_alter` 移除后留下的残枝。同一文件里两支教并存时，要在手册里点明「另一支未接线」。

## 7. 与既有图的接口

- `docs/diagrams/03-main-flow/` 就是本文第 1 节的 22 节点版；本手册的每族流程图从
  「族实现函数被调用」这一节点开始画，**不重复**画 CLI 段。
- `docs/diagrams/04-kernel-flow/` 的五条内核链，在每族手册的「与内核链的对应」一节里指认到具体一条
  （矩阵系 / ω-Y·MrSS 系 / X-Y 系 / 序数系 / 基础序列系）。
- 验收口径（与 C++ 逐字节一致、错误文本照抄）见 `docs/CONTRACT.md`。
