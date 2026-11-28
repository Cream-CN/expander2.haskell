-- | 唯一入口：简陋 CLI，全部参数具名，表达式从 --file 指定，缺省读 stdin。
--
--   expander2-hs --list
--   expander2-hs --id <id> [--term N] [--param N] [--file <path>]
--
-- 每个记号只有一种展开（对齐 C++ dev：FS / FS_alter 已移除，不做通用截断）。
module Main (main) where

import OmegaY.Notation.Types (Notation (..), expandNotation)
import OmegaY.Registry (allNotations, byId)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Opts = Opts
  { optHelp :: Bool
  , optList :: Bool
  , optId :: Maybe String
  , optTerm :: Int
  , optParam :: Int
  , optFile :: Maybe FilePath
  }

opts0 :: Opts
opts0 =
  Opts
    { optHelp = False
    , optList = False
    , optId = Nothing
    , optTerm = 0
    , optParam = 0
    , optFile = Nothing
    }

main :: IO ()
main = do
  args <- getArgs
  case collect opts0 args of
    Left err -> hPutStrLn stderr err >> usage >> exitFailure
    Right o
      | optHelp o -> usage
      | optList o -> mapM_ (\n -> putStrLn (nId n ++ "\t" ++ nName n)) allNotations
      | otherwise ->
          case optId o of
            Nothing -> hPutStrLn stderr "缺少 --id" >> usage >> exitFailure
            Just ident -> do
              text <- case optFile o of
                Just path -> readFile path
                Nothing -> getContents
              case byId ident of
                Nothing -> putStrLn "ERR_NO_NOTATION" >> exitFailure
                Just n ->
                  case expandNotation n text (optTerm o) (optParam o) of
                    Left err -> putStrLn ("ERR:" ++ err)
                    Right out -> putStrLn out

-- | 只认 --flag value、--list、--help；任何不认识的参数都直接报错，不做猜测。
collect :: Opts -> [String] -> Either String Opts
collect o [] = Right o
collect o ("--help" : xs) = collect o {optHelp = True} xs
collect o ("--list" : xs) = collect o {optList = True} xs
collect o ("--id" : v : xs) = collect o {optId = Just v} xs
collect o ("--term" : v : xs) = do
  n <- intArg "--term" v
  collect o {optTerm = n} xs
collect o ("--param" : v : xs) = do
  n <- intArg "--param" v
  collect o {optParam = n} xs
collect o ("--file" : v : xs) = collect o {optFile = Just v} xs
collect o (a : _)
  | take 2 a == "--" = Left ("未知参数：" ++ a)
collect o (a : _) = Left ("多余的位置参数：" ++ a)

intArg :: String -> String -> Either String Int
intArg flag v = case reads v of
  [(n, "")] -> Right n
  _ -> Left (flag ++ " 需要整数：" ++ v)

usage :: IO ()
usage =
  mapM_
    (hPutStrLn stderr)
    [ "usage: expander2-hs --id <id> [--term N] [--param N] [--file <path>]"
    , "       expander2-hs --list"
    , "未给 --file 时表达式从 stdin 读。"
    ]
