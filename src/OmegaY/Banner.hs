{-# LANGUAGE TemplateHaskell #-}

-- | 启动横幅：标志文本、构建时间（UTC+0）、Unix 时间戳、构建工具链、
--   版权与 GPLv3 许可声明。
--
--   输出通道与篇幅的取舍：
--
--     * 交互式会话（REPL）以及 @--help@ / @--version@：完整横幅写 stdout，给人看；
--     * 带数据的 CLI 调用（@<id> <fs|alter> <term>@、@--list@ 等）：只写短的许可声明到 stderr，
--       既满足 GPLv3 的提示要求，又不把日志糊满启动信息。
--     * 设环境变量 @EXPANDER2_NO_BANNER@（取值 @1@ \/ @on@ \/ @true@ \/ @yes@）可静音，
--       便于脚本化调用。
--
--   构建时间由 TemplateHaskell 在本模块编译时取值（GHC 进程内 @getCurrentTime@），
--   即本模块目标文件的生成时刻；增量构建若没有重编该模块，时间会停留在上一次编译它的时刻，
--   全量重建（@build.ps1@ \/ @cmake --build ... --clean-first@）才会刷新。
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

-- | 版权行：GPLv3 要求分发时保留适当的版权声明。
copyrightLine :: String
copyrightLine = "Copyright (C) 2026 王强,Cream-CN,Wty250,Wtydsb,CCFdsb,YLC1145"

-- | 标志文本（取代字符画）：两个 lambda 加 @=Haskell@。
logoText :: [String]
logoText = ["λλ=Haskell"]

trim :: String -> String
trim = dropWhileEndSpace . dropWhile isSpace
 where
  dropWhileEndSpace = reverse . dropWhile isSpace . reverse

-- 编译期取值：可读时间与时间戳在同一秒内一次抓取，二者严格对应。
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

-- 工具链版本：优先问编译时 PATH 上的 @ghc --numeric-version@（含补丁号，如 9.14.1），
-- 拿不到则退回 @System.Info.compilerVersion@（可能只有主次版本）。
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

-- | 许可与担保声明（含 GPLv3 的无担保条款）。
licenseBlock :: [String]
licenseBlock =
  [ "本项目以 GNU 通用公共许可证第 3 版（GPLv3）发布，是自由软件。"
  , "你可以自由运行、研究、再分发与改进它；再分发（含修改版）须同样以 GPLv3 授权。"
  , "完整条款见随附的 LICENSE 文件，或 <https://www.gnu.org/licenses/gpl-3.0.html>。"
  , "本程序不提供任何担保（NO WARRANTY），包括适销性与特定用途适用性的暗示担保。"
  , "原始项目：Cream-CN/Expander2；本仓库为其 Haskell 移植。"
  ]

-- | 构建元信息三行（标签按汉字两列宽对齐到第 10 列）。
buildInfoLines :: [String]
buildInfoLines =
  [ "构建时间  " ++ buildUtc ++ " UTC+0"
  , "时间戳    " ++ show buildStamp
  , "工具链    GHC " ++ ghcVersionText ++ " · " ++ arch ++ "-" ++ os
  ]

-- | 版权行 + 许可声明（不含标志与构建信息）。
licenseNotice :: String
licenseNotice =
  unlines
    ( (programName ++ " " ++ programVersion ++ "  " ++ copyrightLine)
        : licenseBlock
    )

-- | 完整横幅：标志 + 构建信息 + 空行 + 版权与许可（以换行结尾，可直接 hPutStr）。
startupBanner :: String
startupBanner =
  unlines
    ( logoText
        ++ buildInfoLines
        ++ [""]
        ++ [programName ++ " " ++ programVersion ++ "  " ++ copyrightLine]
        ++ licenseBlock
    )

-- | 横幅是否静音：@EXPANDER2_NO_BANNER@ 取值 1/on/true/yes 时为 True。
bannerEnabled :: IO Bool
bannerEnabled = do
  mv <- lookupEnv "EXPANDER2_NO_BANNER"
  pure $ case fmap (map toLower . trim) mv of
    Just v -> v `notElem` ["1", "on", "true", "yes"]
    Nothing -> True

-- | 往指定句柄打印完整横幅（被静音时什么都不做）。
printBannerIfEnabled :: Handle -> IO ()
printBannerIfEnabled h = withBanner h startupBanner

-- | CLI 数据路径的提示：只写短的许可声明到 stderr，不污染 stdout。
announceCli :: IO ()
announceCli = withBanner stderr licenseNotice

withBanner :: Handle -> String -> IO ()
withBanner h txt = do
  on <- bannerEnabled
  if on then hPutStr h txt else pure ()
