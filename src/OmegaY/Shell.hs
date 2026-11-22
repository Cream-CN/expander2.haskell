-- | 供人使用的 CLI/REPL 外壳；计算语义仍由 OmegaY.Registry 与 expandNotation 决定。
--   展开前先强制求值，并可用异步异常中断发散输入。
module OmegaY.Shell (main, repl) where

import Control.Exception
  ( AsyncException (UserInterrupt)
  , IOException
  , SomeException
  , displayException
  , evaluate
  , fromException
  , try
  )
import Control.Monad (forM_, unless, when)
import Data.Char (GeneralCategory (..), generalCategory, isSpace, ord, toLower)
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, nub, transpose)
import Data.Maybe (fromMaybe, isNothing)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Numeric (showHex)
import qualified OmegaY.Banner as Banner
import qualified OmegaY.License as License
import OmegaY.Notation.Types
import OmegaY.Parse (parseIntStrict)
import OmegaY.Registry (allNotations, byId, missingIds)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import System.Info (os)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- 设置

data Settings = Settings
  { stId :: String
  , stMode :: ExpandMode
  , stTerm :: String -- ^ 保留原始文本：界面校验层按 std::stoi 语义解析
  , stParam :: String
  , stUi :: Bool
  , stTimeout :: Int -- ^ 秒，0 表示不限
  , stMaxOut :: Int -- ^ 仅影响显示；0 表示不截断
  }

data Entry = Entry
  { eInput :: String
  , eResult :: Either String String
  , eSecs :: Double
  }

type Hist = [Entry]

defaultSettings :: Settings
defaultSettings =
  case byId "pps" of
    Just _ -> Settings "pps" Fs "1" "0" False 0 0
    Nothing ->
      let n = head allNotations
       in Settings (nId n) Fs "1" "0" False 0 0

nota :: Settings -> Notation
nota st = fromMaybe (head allNotations) (byId (stId st))

modeName :: ExpandMode -> String
modeName Fs = "fs"
modeName FsAlter = "alter"

capsName :: Caps -> String
capsName SeqCaps {} = "seq"
capsName TextCaps {} = "text"
capsName ParamCaps {} = "text_n"

capsSuffix :: Caps -> String
capsSuffix (SeqCaps _ s) = s
capsSuffix (TextCaps _ _ s) = s
capsSuffix (ParamCaps _ _ s) = s

capsHasAlter :: Caps -> Bool
capsHasAlter SeqCaps {} = False
capsHasAlter (TextCaps _ a _) = maybe False (const True) a
capsHasAlter (ParamCaps _ a _) = maybe False (const True) a

paramText :: Maybe (Int, Int, Int, String) -> String
paramText Nothing = "-"
paramText (Just (mn, mx, d, lbl)) =
  show mn ++ ".." ++ show mx ++ "  默认 " ++ show d ++ "  " ++ lbl

tagsText :: [String] -> String
tagsText [] = "-"
tagsText ts = intercalate ", " ts

suffixText :: String -> String
suffixText s
  | null s = "\"\""
  | otherwise = show s

-- 表格排版（按终端显示宽度对齐）

charDisplayWidth :: Char -> Int
charDisplayWidth c
  | generalCategory c `elem` [NonSpacingMark, EnclosingMark, Format, Control] = 0
  | isWideChar c = 2
  | otherwise = 1

isWideChar :: Char -> Bool
isWideChar c =
  let o = ord c
   in (o >= 0x1100 && o <= 0x115F)
        || o == 0x2329
        || o == 0x232A
        || (o >= 0x2E80 && o <= 0x3247 && o /= 0x303F)
        || (o >= 0x3250 && o <= 0x4DBF)
        || (o >= 0x4E00 && o <= 0xA4C6)
        || (o >= 0xA960 && o <= 0xA97C)
        || (o >= 0xAC00 && o <= 0xD7A3)
        || (o >= 0xF900 && o <= 0xFAFF)
        || (o >= 0xFE10 && o <= 0xFE19)
        || (o >= 0xFE30 && o <= 0xFE6B)
        || (o >= 0xFF01 && o <= 0xFF60)
        || (o >= 0xFFE0 && o <= 0xFFE6)
        || (o >= 0x1F300 && o <= 0x1F64F)
        || (o >= 0x1F900 && o <= 0x1F9FF)
        || (o >= 0x20000 && o <= 0x3FFFD)

textWidth :: String -> Int
textWidth = sum . map charDisplayWidth

padWidth :: Int -> String -> String
padWidth w s = s ++ replicate (max 0 (w - textWidth s)) ' '

renderTable :: [[String]] -> [String]
renderTable [] = []
renderTable (hdr : rows) =
  let widths = map (maximum . map textWidth) (transpose (hdr : rows))
      line r = dropWhileEnd (== ' ') (intercalate "  " (zipWith padWidth widths r))
      sep = replicate (sum widths + 2 * (length widths - 1)) '-'
   in line hdr : sep : map line rows

notationTable :: [Notation] -> String
notationTable ns =
  unlines
    ( renderTable
        ( ["ID", "名称", "标签", "能力"]
            : [[nId n, nName n, tagsText (nTags n), capsName (nCaps n)] | n <- ns]
        )
    )

notationLines :: [Notation] -> String
notationLines = unlines . map (\n -> nId n ++ "\t" ++ nName n)

-- | 按当前设置展开：@ui@ 打开时先走 Expander.cpp 那套输入校验，
--   否则直接调用 @expandNotation@。
expandWith :: Notation -> Settings -> String -> Either String String
expandWith n st text
  | stUi st = uiExpand n (stMode st) (stTerm st) (stParam st) text
  | otherwise =
      case parseIntStrict (stTerm st) of
        Nothing -> Left "项数必须是整数"
        Just term ->
          case nCaps n of
            ParamCaps {} ->
              case parseIntStrict (stParam st) of
                Nothing -> Left "参数必须是整数"
                Just prm -> expandNotation n (stMode st) text term prm
            _ -> expandNotation n (stMode st) text term 0

data Outcome = Outcome
  { outResult :: Either String String
  , outSecs :: Double
  }

-- | 强制求值 + 异常兜底 + 可选超时。纯计算里的异常（error / 栈溢出）与发散
--   都在这里被收成 @Left@，REPL 不会因此退出。
runExpand :: Settings -> String -> IO Outcome
runExpand st text = do
  t0 <- getCurrentTime
  r <- exec
  t1 <- getCurrentTime
  pure (Outcome r (realToFrac (diffUTCTime t1 t0)))
  where
    act :: IO (Either String String)
    act = case expandWith (nota st) st text of
      Left e -> pure (Left e)
      Right out -> do
        _ <- evaluate (length out)
        pure (Right out)

    lim = stTimeout st

    -- timeout 必须直接包住 act：System.Timeout 靠捕获内部的 Timeout 异常判定超时，
    -- 中间层若先吞异常，超时信号就失效。其它异常统一在外层归类。
    body
      | lim <= 0 = act
      | otherwise = do
          mt <- timeout (lim * 1000000) act
          pure $ case mt of
            Just x -> x
            Nothing ->
              Left
                ( "展开超时（" ++ show lim ++ " 秒），已中断；"
                    ++ "可用 :timeout 调整或关闭"
                )

    exec = do
      r <- try body :: IO (Either SomeException (Either String String))
      pure $ case r of
        Right x -> x
        Left e
          | Just UserInterrupt <- fromException e -> Left "已中断"
          | otherwise -> Left ("内部异常: " ++ displayException e)

-- 文本工具

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

lower :: String -> String
lower = map toLower

splitWord :: String -> (String, String)
splitWord s =
  let (w, rest) = break isSpace s
   in (w, trim rest)

splitLines :: String -> [String]
splitLines s = map (dropWhileEnd (== '\r')) (lines s)

dropBom :: String -> String
dropBom ('\xFEFF' : rest) = rest
dropBom s = s

trimTrailingNL :: String -> String
trimTrailingNL = reverse . dropWhile (\c -> c == '\n' || c == '\r') . reverse

prepareInput :: Bool -> String -> String
prepareInput verbatim s0 =
  let s1 = dropBom s0
   in if verbatim then s1 else trimTrailingNL s1

flattenText :: String -> String
flattenText = trim . map toSpace
  where
    toSpace c
      | c `elem` "\r\n\t" = ' '
      | otherwise = c

abbreviate :: Int -> String -> String
abbreviate k s
  | length s <= k = s
  | otherwise = take (k - 3) s ++ "..."

clip :: Int -> String -> String
clip n s
  | n <= 0 = s
  | otherwise = take n s

fmtSecs :: Double -> String
fmtSecs s =
  let n = round (s * 100) :: Int
      frac = n `mod` 100
      fracTxt = if frac < 10 then "0" ++ show frac else show frac
   in show (n `div` 100) ++ "." ++ fracTxt

escErr :: String -> String
escErr = concatMap one
  where
    one '\n' = "\\n"
    one '\\' = "\\\\"
    one c = [c]

indentErr :: String -> String
indentErr = intercalate "\n     " . lines

jstr :: String -> String
jstr s = '"' : concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c
      | fromEnum c < 0x20 = "\\u" ++ pad4 (showHex (fromEnum c) "")
      | otherwise = [c]
    pad4 h = replicate (4 - length h) '0' ++ h

jbool :: Bool -> String
jbool b = if b then "true" else "false"

joinPath :: FilePath -> FilePath -> FilePath
joinPath a b = a ++ (if os == "mingw32" then "\\" else "/") ++ b

-- 括号未闭合时自动续行；用于直接粘贴多行矩阵（也可用 :paste）
bracketDepth :: String -> Int
bracketDepth = foldl step 0
  where
    step n c = case c of
      '(' -> n + 1
      '[' -> n + 1
      ')' -> max 0 (n - 1)
      ']' -> max 0 (n - 1)
      _ -> n

-- IO：Windows 控制台切 UTF-8；管道/重定向按 UTF-8 读写

setupIO :: IO ()
setupIO = do
  isTty <- hIsTerminalDevice stdout
  if isTty && os == "mingw32"
    then do
      r <-
        try (readProcessWithExitCode "chcp.com" ["65001"] "")
          :: IO (Either IOException (ExitCode, String, String))
      case r of
        Right (ExitSuccess, _, _) -> setUtf8
        _ -> pure ()
    else setUtf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

setUtf8 :: IO ()
setUtf8 = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

readFileUtf8 :: FilePath -> IO String
readFileUtf8 p = do
  h <- openFile p ReadMode
  hSetEncoding h utf8
  s <- hGetContents h
  _ <- evaluate (length s)
  hClose h
  pure s

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 p s = do
  h <- openFile p WriteMode
  hSetEncoding h utf8
  hPutStr h s
  hClose h

readSourceText :: Maybe FilePath -> IO String
readSourceText Nothing = getContents
readSourceText (Just p) = do
  r <- try (readFileUtf8 p) :: IO (Either IOException String)
  case r of
    Right s -> pure s
    Left e -> do
      hPutStrLn stderr ("无法读取文件: " ++ p ++ "（" ++ show e ++ "）")
      exitWith (ExitFailure 2)

lookupNotation :: String -> IO Notation
lookupNotation ident =
  case byId ident of
    Just n -> pure n
    Nothing -> do
      hPutStrLn stderr "ERR_NO_NOTATION"
      exitWith (ExitFailure 1)

resolveNotation :: String -> Either [Notation] Notation
resolveNotation i =
  case byId i of
    Just n -> Right n
    Nothing ->
      let hits = filter (\n -> lower i `isPrefixOf` lower (nId n)) allNotations
       in case hits of
            [n] -> Right n
            _ -> Left hits

-- 持久历史（~/.expander2_history，仅记录表达式，失败静默）

historyFilePath :: IO FilePath
historyFilePath = do
  mp <- lookupEnv "USERPROFILE"
  home <- case mp of
    Just h | not (null h) -> pure h
    _ -> do
      mh <- lookupEnv "HOME"
      pure (fromMaybe "." mh)
  pure (joinPath home ".expander2_history")

appendHistory :: String -> IO ()
appendHistory line = do
  hp <- historyFilePath
  _ <- try (appendLine hp line) :: IO (Either IOException ())
  pure ()
  where
    appendLine p l = do
      h <- openFile p AppendMode
      hSetEncoding h utf8
      hPutStrLn h l
      hClose h

loadHistory :: IO String
loadHistory = do
  hp <- historyFilePath
  r <- try (readFileUtf8 hp) :: IO (Either IOException String)
  pure (either (const "") id r)

-- CLI

data Opts = Opts
  { oMode :: ExpandMode
  , oTerm :: String
  , oParam :: String
  , oUi :: Bool
  , oTimeout :: Int
  , oJson :: Bool
  , oGrep :: Maybe String
  , oTag :: Maybe String
  , oVerbatim :: Bool
  , oFailFast :: Bool
  , oStrict :: Bool
  , oQuiet :: Bool
  , oPlain :: Bool
  }

defaultOpts :: Opts
defaultOpts = Opts Fs "1" "0" False 0 False Nothing Nothing False False False False False

settingsOf :: Opts -> String -> Settings
settingsOf o ident = Settings ident (oMode o) (oTerm o) (oParam o) (oUi o) (oTimeout o) 0

normalizeFlag :: String -> String
normalizeFlag a = case a of
  "-m" -> "--mode"
  "-t" -> "--term"
  "-p" -> "--param"
  "-q" -> "--quiet"
  _ -> a

parseOpts :: [String] -> Either String (Opts, [String])
parseOpts = go defaultOpts []
  where
    go o pos [] = Right (o, reverse pos)
    go o pos (a0 : as) =
      let a = normalizeFlag a0
          withVal k = case as of
            [] -> Left ("选项 " ++ a0 ++ " 缺少取值")
            (v : as') -> k v o >>= \o' -> go o' pos as'
          switch o' = go o' pos as
       in case a of
            "--mode" ->
              withVal
                ( \v o' ->
                    if v `elem` ["fs", "alter"]
                      then Right o' {oMode = if v == "alter" then FsAlter else Fs}
                      else Left "模式必须是 fs 或 alter"
                )
            "--term" -> withVal (\v o' -> Right o' {oTerm = v})
            "--param" -> withVal (\v o' -> Right o' {oParam = v})
            "--timeout" ->
              withVal
                ( \v o' -> case parseIntStrict v of
                    Just n | n >= 0 -> Right o' {oTimeout = n}
                    _ -> Left "--timeout 需要非负整数（秒，0 表示不限）"
                )
            "--tag" -> withVal (\v o' -> Right o' {oTag = Just v})
            "--grep" -> withVal (\v o' -> Right o' {oGrep = Just v})
            "--ui" -> switch o {oUi = True}
            "--json" -> switch o {oJson = True}
            "--verbatim" -> switch o {oVerbatim = True}
            "--fail-fast" -> switch o {oFailFast = True}
            "--strict" -> switch o {oStrict = True}
            "--quiet" -> switch o {oQuiet = True}
            "--plain" -> switch o {oPlain = True}
            _
              | "--" `isPrefixOf` a -> Left ("未知选项 " ++ a0)
              | otherwise -> go o (a0 : pos) as

optsOrDie :: [String] -> IO (Opts, [String])
optsOrDie args =
  case parseOpts args of
    Left e -> dieUsage e
    Right r -> pure r

shortUsage :: String
shortUsage =
  unlines
    [ "用法: expander2-shell <命令> [选项]"
    , "  list [过滤词]            列出记号（对齐表格；--plain 输出 id<TAB>显示名）"
    , "  show <id>                查看记号元数据"
    , "  run <id> [表达式]        展开一段表达式（省略则从 stdin 读）"
    , "  batch <id> [文件]        逐行展开文件（省略则读 stdin），输出 行号<TAB>输入<TAB>结果"
    , "  repl                     进入交互式模式"
    , "  help / version           帮助 / 版本"
    , "  license                  打印完整 GPLv3 许可证文本（编译期内嵌自 LICENSE）"
    , "选项:"
    , "  -m/--mode fs|alter       展开模式（默认 fs）"
    , "  -t/--term N              项数（默认 1）"
    , "  -p/--param N             参数（默认 0）"
    , "  --ui                     走界面级校验（Expander.cpp 的提示语与判定顺序）"
    , "  --timeout N              单次展开超时秒数，0 表示不限（默认 0）"
    , "  --json                   输出 JSON Lines / JSON 数组"
    , "  --plain                  list 输出旧版 id<TAB>显示名（默认对齐表格）"
    , "  --grep P --tag T         list 的过滤条件"
    , "  --verbatim               不做行尾换行裁剪（默认裁掉结尾的 \\r \\n）"
    , "  --fail-fast              batch 遇到错误立即停止"
    , "  --strict                 展开失败时退出码为 1"
    , "  -q/--quiet               成功时不打印结果（batch/run）"
    ]

helpText :: String
helpText =
  "expander2 shell —— 交互式 + CLI 壳子\n"
    ++ versionLine
    ++ "\n"
    ++ shortUsage
    ++ "\n交互模式命令（行首为 : ）:\n"
    ++ unlines
      [ "  :help / :h / :?        显示本帮助"
      , "  :license / :show license 打印完整 GPLv3 许可证文本（编译期内嵌）"
      , "  :list [过滤词]          列出全部记号（对齐表格，匹配 id / 名称 / 标签）"
      , "  :show [id|前缀]         查看记号元数据（缺省为当前记号）"
      , "  :use <id|前缀>          切换当前记号（唯一前缀可省略后半段）"
      , "  :mode fs|alter         切换展开模式"
      , "  :term <整数>            设置项数（界面校验时按 std::stoi 语义解析）"
      , "  :param <整数>           设置参数（仅参数化记号使用）"
      , "  :ui on|off             开关界面级校验"
      , "  :timeout <秒|off>       单次展开超时（防止发散输入把 REPL 卡死）"
      , "  :maxout <字符数|off>    结果打印截断上限（:save 仍写全文）"
      , "  :x <表达式>             按当前设置展开（同义 :expand）"
      , "  :paste                 多行粘贴，单独一行 . 结束"
      , "  :read <文件>            读整个文件（换行视作空格）后展开"
      , "  :each <文件>            对文件每一行按当前设置展开"
      , "  :save <文件>            把上一次成功的结果写入文件（UTF-8）"
      , "  :last                  显示上一次的输入、结果与耗时"
      , "  :history [n]           显示最近 n 条历史（默认 10）"
      , "  :log [n]               查看磁盘历史 ~/.expander2_history"
      , "  :status                显示当前设置"
      , "  :tags                  列出全部标签"
      , "  :reset                 重置设置（pps / fs / term=1 / param=0）"
      , "  :quit / :q             退出（Ctrl-D 亦可）"
      , ""
      , "直接输入表达式（不以 : 开头）即按当前设置展开；括号未闭合会自动续行，"
      , "Ctrl-C 取消当前输入，空行忽略。"
      ]

versionLine :: String
versionLine =
  "expander2-shell 0.1.1   已注册 "
    ++ show (length allNotations)
    ++ " / "
    ++ show (length allNotations + length missingIds)
    ++ " 个记号"

-- | @--version@ 输出许可证横幅与版本行。
versionText :: String
versionText = Banner.startupBanner ++ versionLine ++ "\n"

dieUsage :: String -> IO a
dieUsage msg = do
  hPutStrLn stderr ("错误: " ++ msg)
  hPutStrLn stderr ""
  hPutStrLn stderr shortUsage
  exitWith (ExitFailure 2)

dispatch :: String -> [String] -> IO ()
dispatch c rest = case c of
  "list" -> do
    (o, pos) <- optsOrDie rest
    cmdList o pos
  "show" -> do
    (_o, pos) <- optsOrDie rest
    cmdShow pos
  "run" -> do
    (o, pos) <- optsOrDie rest
    cmdRun o pos
  "batch" -> do
    (o, pos) <- optsOrDie rest
    cmdBatch o pos
  "repl" -> repl
  "shell" -> repl
  "interactive" -> repl
  "help" -> putStr helpText
  "version" -> putStr versionText
  "license" -> putStr License.licenseText
  "licence" -> putStr License.licenseText
  "gpl" -> putStr License.licenseText
  _ -> dieUsage ("未知命令: " ++ c)

matches :: Maybe String -> Maybe String -> Notation -> Bool
matches pat mtag n =
  let hay = lower (nId n ++ " " ++ nName n ++ " " ++ unwords (nTags n))
      okPat = maybe True (\p -> lower p `isInfixOf` hay) pat
      okTag = maybe True (\t -> lower t `elem` map lower (nTags n)) mtag
   in okPat && okTag

cmdList :: Opts -> [String] -> IO ()
cmdList o pos = do
  let pat = case pos of
        (p : _) -> Just p
        _ -> oGrep o
      hits = filter (matches pat (oTag o)) allNotations
  if null hits
    then hPutStrLn stderr "没有匹配的记号"
    else
      if oJson o
        then
          putStrLn
            ( "["
                ++ intercalate
                  ","
                  [ "{\"id\":" ++ jstr (nId n)
                      ++ ",\"name\":"
                      ++ jstr (nName n)
                      ++ ",\"caps\":"
                      ++ jstr (capsName (nCaps n))
                      ++ "}"
                  | n <- hits
                  ]
                ++ "]"
            )
        else
          if oPlain o
            then putStr (notationLines hits)
            else putStr (notationTable hits)

cmdShow :: [String] -> IO ()
cmdShow pos = case pos of
  ("license" : _) -> putStr License.licenseText
  [] -> dieUsage "show 需要 <id>"
  (i : _) -> case resolveNotation i of
    Right n -> putStr (renderNotation n)
    Left [] -> do
      hPutStrLn stderr ("未知记号: " ++ i)
      exitWith (ExitFailure 1)
    Left hits -> do
      hPutStrLn stderr ("前缀 " ++ show i ++ " 匹配多个记号，请写全：")
      hPutStr stderr (notationTable hits)
      exitWith (ExitFailure 2)

renderNotation :: Notation -> String
renderNotation n =
  unlines
    [ "id       " ++ nId n
    , "name     " ++ nName n
    , "tags     " ++ tagsText (nTags n)
    , "caps     " ++ capsName (nCaps n)
    , "param    " ++ paramText (nParam n)
    , "alter    "
        ++ if capsHasAlter (nCaps n)
          then "独立实现"
          else "无（alter 模式回退到 fs 算法）"
    , "suffix   " ++ suffixText (capsSuffix (nCaps n))
    ]

jsonRecord :: Maybe Int -> String -> Opts -> String -> Either String String -> String
jsonRecord mline ident o input r =
  let flds =
        maybe [] (\k -> ["\"line\":" ++ show k]) mline
          ++ [ "\"id\":" ++ jstr ident
             , "\"mode\":" ++ jstr (modeName (oMode o))
             , "\"term\":" ++ jstr (oTerm o)
             , "\"param\":" ++ jstr (oParam o)
             , "\"ui\":" ++ jbool (oUi o)
             , "\"in\":" ++ jstr input
             ]
          ++ case r of
            Right out -> ["\"ok\":true", "\"out\":" ++ jstr out]
            Left e -> ["\"ok\":false", "\"err\":" ++ jstr e]
   in "{" ++ intercalate "," flds ++ "}"

emitOne :: Opts -> Notation -> String -> Either String String -> IO ()
emitOne o n input r
  | oJson o = putStrLn (jsonRecord Nothing (nId n) o input r)
  | otherwise = case r of
      Right out -> if oQuiet o then pure () else putStrLn out
      Left e -> putStrLn ("ERR:" ++ escErr e)

cmdRun :: Opts -> [String] -> IO ()
cmdRun o pos = do
  (ident, inline) <- case pos of
    [] -> dieUsage "run 需要 <id>"
    (i : ts) -> pure (i, if null ts then Nothing else Just (unwords ts))
  n <- lookupNotation ident
  raw <- case inline of
    Just t -> pure t
    Nothing -> getContents
  let text = prepareInput (oVerbatim o) raw
      st = settingsOf o ident
  oc <- runExpand st text
  emitOne o n text (outResult oc)
  when (oStrict o && isLeft (outResult oc)) $ exitWith (ExitFailure 1)

cmdBatch :: Opts -> [String] -> IO ()
cmdBatch o pos = do
  (ident, mpath) <- case pos of
    [] -> dieUsage "batch 需要 <id>"
    (i : rest) -> pure (i, case rest of (f : _) -> Just f; _ -> Nothing)
  n <- lookupNotation ident
  src <- readSourceText mpath
  let st = settingsOf o ident
      inp t = prepareInput (oVerbatim o) t
      rows =
        [ (k, t)
        | (k, ln) <- zip [1 :: Int ..] (splitLines src)
        , let t = trim ln
        , not (null t)
        ]
  if oJson o
    then do
      rs <- mapM (\(k, t) -> do oc <- runExpand st (inp t); pure (k, inp t, outResult oc)) rows
      putStrLn
        ( "["
            ++ intercalate ","
              [jsonRecord (Just k) ident o t r | (k, t, r) <- rs]
            ++ "]"
        )
      when (oStrict o && any (\(_, _, r) -> isLeft r) rs) $ exitWith (ExitFailure 1)
    else do
      errRef <- newIORef False
      forM_ rows $ \(k, t) -> do
        oc <- runExpand st (inp t)
        case outResult oc of
          Right out -> unless (oQuiet o) $ putStrLn (show k ++ "\t" ++ inp t ++ "\t" ++ out)
          Left e -> do
            putStrLn (show k ++ "\t" ++ inp t ++ "\tERR:" ++ escErr e)
            writeIORef errRef True
            when (oFailFast o) $ exitWith (ExitFailure 1)
      strict <- readIORef errRef
      when (oStrict o && strict) $ exitWith (ExitFailure 1)

-- REPL

repl :: IO ()
repl = do
  Banner.printBannerIfEnabled stdout
  putStrLn versionLine
  putStrLn "输入 :help 查看命令，:quit 退出；直接输入表达式即按当前设置展开。"
  putStr (renderStatus defaultSettings)
  loop defaultSettings []

prompt :: Settings -> String
prompt st =
  "["
    ++ stId st
    ++ " "
    ++ modeName (stMode st)
    ++ " t="
    ++ stTerm st
    ++ " p="
    ++ stParam st
    ++ (if stUi st then " ui" else "")
    ++ "]> "

data Input = LineIn String | EofIn | Interrupted

getInput :: IO Input
getInput = do
  r <- try (hGetLine stdin) :: IO (Either SomeException String)
  case r of
    Right s -> pure (LineIn s)
    Left e
      | Just UserInterrupt <- fromException e -> pure Interrupted
      | otherwise -> pure EofIn

loop :: Settings -> Hist -> IO ()
loop st hist = do
  putStr (prompt st)
  hFlush stdout
  mi <- getInput
  case mi of
    EofIn -> putStrLn "\nbye."
    Interrupted -> do
      putStrLn ""
      loop st hist
    LineIn line0 -> do
      let line = trim line0
      if null line
        then loop st hist
        else
          if head line == ':'
            then do
              (mst, hist') <- command st hist line
              case mst of
                Nothing -> pure ()
                Just st' -> loop st' hist'
            else do
              mexpr <- readExpression line
              case mexpr of
                Nothing -> loop st hist -- 续行时被 Ctrl-C 取消
                Just expr -> do
                  let full = trim expr
                  hist' <- evalLine st full hist
                  loop st hist'

-- 括号未闭合时继续读行（普通输入也能粘多行，不必先 :paste）；
-- 续行阶段按 Ctrl-C / Ctrl-D 直接放弃这条表达式。
readExpression :: String -> IO (Maybe String)
readExpression first = go first
  where
    go acc
      | bracketDepth acc <= 0 = pure (Just acc)
      | otherwise = do
          putStr "… "
          hFlush stdout
          mi <- getInput
          case mi of
            LineIn l -> go (acc ++ " " ++ trim l)
            _ -> pure Nothing

evalLine :: Settings -> String -> Hist -> IO Hist
evalLine st text hist = do
  oc <- runExpand st text
  report st oc
  appendHistory text
  pure (pushEntry (Entry text (outResult oc) (outSecs oc)) hist)

pushEntry :: Entry -> Hist -> Hist
pushEntry e hist = take 200 (e : hist)

report :: Settings -> Outcome -> IO ()
report st (Outcome r secs) = do
  case r of
    Right out -> do
      putStrLn (clip (stMaxOut st) out)
      when (stMaxOut st > 0 && length out > stMaxOut st) $
        putStrLn
          ( "…（已截断，完整长度 " ++ show (length out)
              ++ " 字符；:maxout off 关闭，:save 写出全文）"
          )
    Left e -> printResult (Left e)
  when (secs >= 0.1) $ putStrLn ("-- " ++ fmtSecs secs ++ " s")

printResult :: Either String String -> IO ()
printResult (Right out) = putStrLn out
printResult (Left e) = putStrLn ("ERR: " ++ indentErr e)

-- | 返回 @Nothing@ 表示退出。
command :: Settings -> Hist -> String -> IO (Maybe Settings, Hist)
command st hist raw = do
  let body = drop 1 (trim raw)
      (c0, rest0) = splitWord body
      c = lower c0
      rest = trim rest0
  case c of
    "" -> do
      putStrLn "缺少命令名（:help 查看命令）"
      pure (Just st, hist)
    "q" -> pure (Nothing, hist)
    "quit" -> pure (Nothing, hist)
    "exit" -> pure (Nothing, hist)
    "h" -> putStr helpText >> pure (Just st, hist)
    "help" -> putStr helpText >> pure (Just st, hist)
    "?" -> putStr helpText >> pure (Just st, hist)
    "version" -> putStr versionText >> pure (Just st, hist)
    "license" -> putStr License.licenseText >> pure (Just st, hist)
    "licence" -> putStr License.licenseText >> pure (Just st, hist)
    "gpl" -> putStr License.licenseText >> pure (Just st, hist)
    "list" -> do
      listCmd rest
      pure (Just st, hist)
    "show" -> do
      showCmd st rest
      pure (Just st, hist)
    "tags" -> do
      tagsCmd
      pure (Just st, hist)
    "use" -> do
      r <- useCmd st rest
      pure (Just r, hist)
    "mode" -> do
      r <- modeCmd st rest
      pure (Just r, hist)
    "term" -> do
      r <- setRaw st rest "项数" "term" (\s v -> s {stTerm = v})
      pure (Just r, hist)
    "param" -> do
      r <- setRaw st rest "参数" "param" (\s v -> s {stParam = v})
      pure (Just r, hist)
    "ui" -> do
      r <- uiCmd st rest
      pure (Just r, hist)
    "timeout" -> do
      r <- timeoutCmd st rest
      pure (Just r, hist)
    "maxout" -> do
      r <- maxOutCmd st rest
      pure (Just r, hist)
    "x" -> expandText st hist rest
    "expand" -> expandText st hist rest
    "e" -> expandText st hist rest
    "paste" -> do
      putStrLn "多行输入，单独一行 . 结束"
      ls <- collectPaste []
      expandText st hist (unwords (filter (not . null) ls))
    "read" -> case words rest of
      [] -> do
        putStrLn "用法: :read <文件>"
        pure (Just st, hist)
      (p : _) -> do
        s <- readSourceText (Just p)
        expandText st hist (flattenText s)
    "each" -> case words rest of
      [] -> do
        putStrLn "用法: :each <文件>"
        pure (Just st, hist)
      (p : _) -> do
        s <- readSourceText (Just p)
        eachCmd st s
        pure (Just st, hist)
    "save" -> do
      saveCmd hist rest
      pure (Just st, hist)
    "last" -> do
      case hist of
        [] -> putStrLn "还没有展开过表达式"
        (e : _) -> do
          putStrLn ("输入: " ++ eInput e)
          printResult (eResult e)
          putStrLn ("耗时 " ++ fmtSecs (eSecs e) ++ " s")
      pure (Just st, hist)
    "history" -> do
      historyCmd hist rest
      pure (Just st, hist)
    "log" -> do
      logCmd rest
      pure (Just st, hist)
    "status" -> do
      putStr (renderStatus st)
      pure (Just st, hist)
    "reset" -> do
      let st' = defaultSettings
      putStrLn "已重置设置"
      putStr (renderStatus st')
      pure (Just st', hist)
    _ -> do
      putStrLn ("未知命令: :" ++ c ++ "（:help 查看命令）")
      pure (Just st, hist)

listCmd :: String -> IO ()
listCmd "" = putStr (notationTable allNotations)
listCmd pat =
  let hits = filter (matches (Just pat) Nothing) allNotations
   in if null hits
        then putStrLn ("没有匹配 " ++ show pat ++ " 的记号")
        else putStr (notationTable hits)

showCmd :: Settings -> String -> IO ()
showCmd st rest = case words rest of
  ("license" : _) -> putStr License.licenseText
  [] -> putStr (renderNotation (nota st))
  (i : _) -> case resolveNotation i of
    Right n -> putStr (renderNotation n)
    Left [] -> putStrLn ("未知记号: " ++ i ++ "（:list 可列出全部）")
    Left hits -> do
      putStrLn ("前缀 " ++ show i ++ " 匹配多个记号：")
      putStr (notationTable hits)

tagsCmd :: IO ()
tagsCmd = do
  let ts = nub [t | n <- allNotations, t <- nTags n]
      cnt t = length [() | n <- allNotations, t `elem` nTags n]
  if null ts
    then putStrLn "没有标签"
    else forM_ ts $ \t -> putStrLn (t ++ "\t" ++ show (cnt t) ++ " 个记号")

useCmd :: Settings -> String -> IO Settings
useCmd st rest = case words rest of
  [] -> do
    putStrLn ("当前记号: " ++ stId st ++ "  " ++ nName (nota st))
    pure st
  (i : _) -> case resolveNotation i of
    Left [] -> do
      putStrLn ("未知记号: " ++ i ++ "（:list 可列出全部）")
      pure st
    Left hits -> do
      putStrLn ("前缀 " ++ show i ++ " 匹配多个记号，请写全：")
      forM_ hits $ \n -> putStrLn ("  " ++ nId n ++ "\t" ++ nName n)
      pure st
    Right n -> do
      putStrLn ("已切换: " ++ nId n ++ "  " ++ nName n ++ "  [" ++ capsName (nCaps n) ++ "]")
      when (stMode st == FsAlter && not (capsHasAlter (nCaps n))) $
        putStrLn "提示: 该记号没有独立 alter 实现，alter 模式将退回 fs 算法"
      case nParam n of
        Just (mn, mx, d, lbl) ->
          putStrLn
            ( "参数范围 " ++ show mn ++ ".." ++ show mx
                ++ "，默认 " ++ show d
                ++ (if null lbl then "" else "（" ++ lbl ++ "）")
            )
        Nothing -> pure ()
      pure st {stId = nId n}

modeCmd :: Settings -> String -> IO Settings
modeCmd st rest = case lower rest of
  "" -> do
    putStrLn ("当前模式: " ++ modeName (stMode st) ++ "（用法: :mode fs|alter）")
    pure st
  "fs" -> do
    putStrLn "模式: fs"
    pure st {stMode = Fs}
  "alter" -> do
    putStrLn "模式: alter"
    when (not (capsHasAlter (nCaps (nota st)))) $
      putStrLn "提示: 当前记号没有独立 alter 实现，展开时回退到 fs 算法"
    pure st {stMode = FsAlter}
  other -> do
    putStrLn ("无效模式: " ++ other ++ "（fs 或 alter）")
    pure st

setRaw :: Settings -> String -> String -> String -> (Settings -> String -> Settings) -> IO Settings
setRaw st rest label cmdName upd = do
  let v = trim rest
  if null v
    then do
      putStrLn ("用法: :" ++ cmdName ++ " <整数>")
      pure st
    else do
      when (isNothing (parseIntStrict v)) $
        putStrLn ("提示: " ++ label ++ "不是整数；关闭界面校验后展开会报“" ++ label ++ "必须是整数”")
      let st' = upd st v
      putStrLn (label ++ " = " ++ v)
      pure st'

uiCmd :: Settings -> String -> IO Settings
uiCmd st rest = case lower rest of
  "" -> do
    putStrLn ("界面校验: " ++ (if stUi st then "on" else "off"))
    pure st
  "on" -> do
    putStrLn "界面校验: on（先做 stoi / 范围 / 空串校验，再展开）"
    pure st {stUi = True}
  "off" -> do
    putStrLn "界面校验: off（直接调用展开层）"
    pure st {stUi = False}
  other -> do
    putStrLn ("无效取值: " ++ other ++ "（on 或 off）")
    pure st

timeoutCmd :: Settings -> String -> IO Settings
timeoutCmd st rest = case lower rest of
  "" -> do
    putStrLn ("当前超时: " ++ (if stTimeout st == 0 then "off" else show (stTimeout st) ++ " 秒"))
    pure st
  "off" -> do
    putStrLn "超时: off（发散输入可能会卡住，Ctrl-C 可中断）"
    pure st {stTimeout = 0}
  other -> case parseIntStrict other of
    Just n
      | n >= 0 -> do
          putStrLn ("超时: " ++ (if n == 0 then "off" else show n ++ " 秒"))
          pure st {stTimeout = n}
    _ -> do
      putStrLn ("无效取值: " ++ other ++ "（非负整数秒，或 off）")
      pure st

maxOutCmd :: Settings -> String -> IO Settings
maxOutCmd st rest = case lower rest of
  "" -> do
    putStrLn ("输出上限: " ++ (if stMaxOut st == 0 then "off" else show (stMaxOut st) ++ " 字符"))
    pure st
  "off" -> do
    putStrLn "输出上限: off"
    pure st {stMaxOut = 0}
  other -> case parseIntStrict other of
    Just n
      | n >= 0 -> do
          putStrLn ("输出上限: " ++ (if n == 0 then "off" else show n ++ " 字符"))
          pure st {stMaxOut = n}
    _ -> do
      putStrLn ("无效取值: " ++ other ++ "（非负整数，或 off）")
      pure st

expandText :: Settings -> Hist -> String -> IO (Maybe Settings, Hist)
expandText st hist text
  | null (trim text) = do
      putStrLn "用法: :x <表达式>   或   :paste 粘贴多行"
      pure (Just st, hist)
  | otherwise = do
      hist' <- evalLine st (trim text) hist
      pure (Just st, hist')

collectPaste :: [String] -> IO [String]
collectPaste acc = do
  putStr "… "
  hFlush stdout
  mi <- getInput
  case mi of
    LineIn l
      | trim l == "." -> pure (reverse acc)
      | otherwise -> collectPaste (trim l : acc)
    _ -> pure (reverse acc)

eachCmd :: Settings -> String -> IO ()
eachCmd st src =
  forM_ (zip [1 :: Int ..] (filter (not . null) (map trim (splitLines src)))) $ \(k, ln) -> do
    oc <- runExpand st ln
    case outResult oc of
      Right out -> putStrLn (show k ++ "\t" ++ ln ++ "\t" ++ out)
      Left e -> putStrLn (show k ++ "\t" ++ ln ++ "\tERR:" ++ escErr e)

saveCmd :: Hist -> String -> IO ()
saveCmd hist rest = case words rest of
  [] -> putStrLn "用法: :save <文件>"
  (p : _) -> case hist of
    [] -> putStrLn "还没有可保存的结果"
    (e : _) -> case eResult e of
      Left _ -> putStrLn "上一次展开失败了，没有可写的结果"
      Right out -> do
        r <- try (writeFileUtf8 p out) :: IO (Either IOException ())
        case r of
          Right () -> putStrLn ("已写入 " ++ show (length out) ++ " 字符到 " ++ p)
          Left ex -> putStrLn ("写入失败: " ++ show ex)

historyCmd :: Hist -> String -> IO ()
historyCmd hist rest = do
  let k = case parseIntStrict (trim rest) of
        Just v -> max 1 v
        Nothing -> 10
      h = take k hist
  if null h
    then putStrLn "历史为空"
    else
      forM_ (zip [1 :: Int ..] (reverse h)) $ \(j, e) ->
        putStrLn
          ( show j ++ ". " ++ abbreviate 100 (eInput e)
              ++ "   =>   "
              ++ abbreviate 120 (either ("ERR: " ++) id (eResult e))
              ++ "  (" ++ fmtSecs (eSecs e) ++ "s)"
          )

logCmd :: String -> IO ()
logCmd rest = do
  hp <- historyFilePath
  s <- loadHistory
  let ls = splitLines s
      n = case parseIntStrict (trim rest) of
        Just v -> max 1 v
        Nothing -> 10
      shown = reverse (take n (reverse ls))
  putStrLn ("历史文件: " ++ hp)
  if null ls
    then putStrLn "（为空：尚未记录表达式）"
    else do
      putStrLn ("最近 " ++ show (length shown) ++ " 条 / 共 " ++ show (length ls) ++ " 条")
      forM_ shown $ \l -> putStrLn ("  " ++ abbreviate 160 l)

renderStatus :: Settings -> String
renderStatus st =
  let n = nota st
      alterNote =
        if stMode st == FsAlter && not (capsHasAlter (nCaps n))
          then "（该记号无独立 alter，将回退 fs）"
          else ""
   in unlines
        [ "记号  " ++ nId n ++ "  (" ++ nName n ++ ")"
        , "类型  " ++ capsName (nCaps n) ++ "  " ++ suffixText (capsSuffix (nCaps n))
        , "参数  " ++ paramText (nParam n)
        , "模式  " ++ modeName (stMode st) ++ alterNote
        , "项数  " ++ stTerm st
        , "超时  " ++ if stTimeout st == 0 then "off" else show (stTimeout st) ++ " 秒"
        , "上限  " ++ if stMaxOut st == 0 then "off" else show (stMaxOut st) ++ " 字符"
        , "校验  "
            ++ if stUi st
              then "on（界面层：stoi / 范围 / 空串）"
              else "off（展开层：expandNotation）"
        ]

-- 入口

main :: IO ()
main = do
  args <- getArgs
  setupIO
  case args of
    ["-h"] -> Banner.printBannerIfEnabled stdout >> putStr helpText
    ["--help"] -> Banner.printBannerIfEnabled stdout >> putStr helpText
    ["--version"] -> putStr versionText
    [] -> do
      tty <- hIsTerminalDevice stdin
      if tty
        then repl
        else Banner.announceCli >> dieUsage "缺少命令"
    (c : rest) -> do
      when (c `notElem` ["repl", "shell", "interactive"]) Banner.announceCli
      dispatch c rest
