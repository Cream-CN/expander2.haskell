-- | 命令行入口：与 C++ 侧对照程序同构。
--
--   @expander2-hs --list@
--   @expander2-hs <id> <fs|alter> <term> [param]@ ，表达式从 stdin 读入
module Main (main) where

import OmegaY.Notation.Types (Caps (..), ExpandMode (..), Notation (..), expandNotation, uiExpand)
import OmegaY.Registry (allNotations, byId, missingIds)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--list"] ->
      mapM_ (\n -> putStrLn (nId n ++ "\t" ++ nName n)) allNotations
    ["--missing"] ->
      mapM_ putStrLn missingIds
    ["--params"] ->
      mapM_ (\n -> putStrLn (nId n ++ "\t" ++ paramDesc (nParam n) ++ "\t" ++ capsDesc (nCaps n))) allNotations
    ("--ui" : ident : modeStr : termStr : rest) -> do
      text <- getContents
      case byId ident of
        Nothing -> putStrLn "ERR_NO_NOTATION" >> exitFailure
        Just n ->
          case uiExpand n (if modeStr == "alter" then FsAlter else Fs)
                        termStr (case rest of (p : _) -> p; _ -> "0") text of
            Left msg -> putStrLn ("ERR:" ++ esc msg)
            Right out -> putStrLn out
    (ident : modeStr : termStr : rest) -> do
      text <- getContents
      let term = read termStr :: Int
          param = case rest of
            (p : _) -> read p :: Int
            _ -> 0
          mode = if modeStr == "alter" then FsAlter else Fs
      case byId ident of
        Nothing -> putStrLn "ERR_NO_NOTATION" >> exitFailure
        Just n ->
          case expandNotation n mode text term param of
            Left err -> putStrLn ("ERR:" ++ err)
            Right out -> putStrLn out
    _ -> do
      hPutStrLn stderr "usage: expander2-hs <id> <fs|alter> <term> [param]  (text on stdin)"
      hPutStrLn stderr "       expander2-hs --ui <id> <fs|alter> <termRaw> [paramRaw]"
      hPutStrLn stderr "       expander2-hs --list"
      exitFailure


-- | 与 refui 预言机同款转义：换行 -> 字面 "\n"，反斜杠 -> "\\"。
esc :: String -> String
esc = concatMap one
 where
  one '\n' = "\\n"
  one '\\' = "\\\\"
  one c = [c]

paramDesc :: Maybe (Int, Int, Int, String) -> String
paramDesc Nothing = "-"
paramDesc (Just (mn, mx, d, _)) = show mn ++ ".." ++ show mx ++ " default=" ++ show d

capsDesc :: Caps -> String
capsDesc (SeqCaps {}) = "seq"
capsDesc (TextCaps {}) = "text"
capsDesc (ParamCaps {}) = "text_n"
