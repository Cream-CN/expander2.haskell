{-# LANGUAGE TemplateHaskell #-}

-- | 启动横幅。构建时间由 TemplateHaskell 在编译本模块时取得，
--   增量构建不会自动刷新；@EXPANDER2_NO_BANNER@ 可关闭全部输出。
module OmegaY.Banner
  ( programName
  , programVersion
  , copyrightLine
  , licenseBlock
  , logoText
  , buildInfoLines
  , licenseNotice
  , startupBanner
  , bannerEnabled
  , printBannerIfEnabled
  , announceCli
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace, toLower)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Version (showVersion)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Info (arch, compilerName, compilerVersion, os)
import System.IO (Handle, hPutStr, stderr)
import System.Process (readProcessWithExitCode)

programName :: String
programName = "expander2"

programVersion :: String
programVersion = "0.1.1"

copyrightLine :: String
copyrightLine = "Copyright (C) 2026 王强,Cream-CN,Wty250,Wtydsb,CCFdsb,YLC1145"

logoText :: [String]
logoText = ["λλ=Haskell"]

trim :: String -> String
trim = dropWhileEndSpace . dropWhile isSpace
 where
  dropWhileEndSpace = reverse . dropWhile isSpace . reverse

-- 编译期一次取时间与时间戳，保证两者严格对应同一秒。
$( do
     t <- runIO getCurrentTime
     let txt = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t
         st = round (utcTimeToPOSIXSeconds t) :: Integer
     [d| buildUtc :: String
         buildUtc = $(lift txt)
         buildStamp :: Integer
         buildStamp = $(lift st)
       |]
 )

-- 优先使用 PATH 上的 ghc --numeric-version（含补丁号）；失败则退回 System.Info。
$( do
     v <- runIO $ do
       r <- try (readProcessWithExitCode "ghc" ["--numeric-version"] "")
             :: IO (Either IOException (ExitCode, String, String))
       pure $ case r of
         Right (ExitSuccess, out, _) ->
           let s = takeWhile (/= '\n') out
               t = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
            in t
         _ -> showVersion compilerVersion
     [d| ghcVersionText :: String
         ghcVersionText = $(lift v)
       |]
 )

licenseBlock :: [String]
licenseBlock =
  [ "本项目以 GNU 通用公共许可证第 3 版（GPLv3）发布，是自由软件。"
  , "你可以自由运行、研究、再分发与改进它；再分发（含修改版）须同样以 GPLv3 授权。"
  , "完整条款见随附的 LICENSE 文件，或 <https://www.gnu.org/licenses/gpl-3.0.html>。"
  , "本程序不提供任何担保（NO WARRANTY），包括适销性与特定用途适用性的暗示担保。"
  , "原始项目：Cream-CN/Expander2；本仓库为其 Haskell 移植。"
  ]

buildInfoLines :: [String]
buildInfoLines =
  [ "构建时间  " ++ buildUtc ++ " UTC+0"
  , "时间戳    " ++ show buildStamp
  , "工具链    GHC " ++ ghcVersionText ++ " · " ++ arch ++ "-" ++ os
  ]

licenseNotice :: String
licenseNotice =
  unlines
    ( (programName ++ " " ++ programVersion ++ "  " ++ copyrightLine)
        : licenseBlock
    )

startupBanner :: String
startupBanner =
  unlines
    ( logoText
        ++ buildInfoLines
        ++ [""]
        ++ [programName ++ " " ++ programVersion ++ "  " ++ copyrightLine]
        ++ licenseBlock
    )

-- 返回值是“启用”而非“静音”：环境变量取 1/on/true/yes 时为 False。
bannerEnabled :: IO Bool
bannerEnabled = do
  mv <- lookupEnv "EXPANDER2_NO_BANNER"
  pure $ case fmap (map toLower . trim) mv of
    Just v -> v `notElem` ["1", "on", "true", "yes"]
    Nothing -> True

printBannerIfEnabled :: Handle -> IO ()
printBannerIfEnabled h = withBanner h startupBanner

announceCli :: IO ()
announceCli = withBanner stderr licenseNotice

withBanner :: Handle -> String -> IO ()
withBanner h txt = do
  on <- bannerEnabled
  if on then hPutStr h txt else pure ()
