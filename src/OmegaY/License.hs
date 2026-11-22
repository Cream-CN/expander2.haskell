{-# LANGUAGE TemplateHaskell #-}

-- | GPLv3 全文：编译期从仓库根的 @LICENSE@ 读入并烘焙进二进制，
--   运行时不再依赖文件是否存在（装到别处也能打印完整许可证）。
--
--   定位策略：先用本模块源文件所在目录回推仓库根（@src/OmegaY@ 往上两级），
--   再退回当前工作目录下的 @LICENSE@；两处都没有则编译失败并给出尝试过的路径。
--   @addDependentFile@ 保证 LICENSE 改动会触发重编。
module OmegaY.License (licenseText) where

import Control.Exception (IOException, try)
import Data.List (intercalate)
import Language.Haskell.TH (location, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift, loc_filename)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

$( do
     loc <- location
     let here = takeDirectory (loc_filename loc)
         cands = [here </> ".." </> ".." </> "LICENSE", "LICENSE"]

         -- 统一换行：CRLF / CR -> LF，并保留结尾换行。
         normalize = unlines . map (filter (/= '\r')) . lines

         tryRead :: FilePath -> IO (Maybe (FilePath, String))
         tryRead p = do
           ok <- doesFileExist p
           if not ok
             then pure Nothing
             else do
               r <- try (readFile p) :: IO (Either IOException String)
               pure (either (const Nothing) (Just . (,) p . normalize) r)

         firstReadable :: [FilePath] -> IO (Maybe (FilePath, String))
         firstReadable [] = pure Nothing
         firstReadable (p : ps) = do
           m <- tryRead p
           case m of
             Just hit -> pure (Just hit)
             Nothing -> firstReadable ps

     pair <- runIO $ do
       m <- firstReadable cands
       case m of
         Just hit -> pure hit
         Nothing ->
           fail ("OmegaY.License: 找不到 LICENSE，已尝试: " ++ intercalate " , " cands)

     addDependentFile (fst pair)

     [d| licenseText :: String
         licenseText = $(lift (snd pair))
       |]
 )
