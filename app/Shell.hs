-- | 壳子入口：交互式 + CLI 逻辑在 @OmegaY.Shell@（便于单独编译与测试）。
module Main (main) where

import qualified OmegaY.Shell as Shell

main :: IO ()
main = Shell.main
