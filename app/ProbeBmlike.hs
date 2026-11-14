-- | 单族差分探针：只装载 bmlike 族，供 test/diff.py 对照预言机。
module Main (main) where
import OmegaY.Fam.Bmlike.Family (familyNotations)
import OmegaY.Notation.Types (ExpandMode (..), Notation (..), expandNotation)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--list"] -> mapM_ (\n -> putStrLn (nId n ++ "\t" ++ nName n)) familyNotations
    (ident : modeStr : termStr : rest) -> do
      text <- getContents
      let term = read termStr :: Int
          param = case rest of (p : _) -> read p :: Int; _ -> 0
          mode = if modeStr == "alter" then FsAlter else Fs
      case lookup ident [(nId n, n) | n <- familyNotations] of
        Nothing -> putStrLn "ERR_NO_NOTATION" >> exitFailure
        Just n -> case expandNotation n mode text term param of
          Left err -> putStrLn ("ERR:" ++ err)
          Right out -> putStrLn out
    _ -> do
      hPutStrLn stderr "usage: probe <id> <fs|alter> <term> [param]"
      exitFailure
