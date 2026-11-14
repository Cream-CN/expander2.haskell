{-# LANGUAGE OverloadedStrings #-}
-- | UP2DBMS v1B+（id: up2dbms-v1b-plus）。
module OmegaY.Fam.Dbms.Up2V1bPlus (expandText, expandAlter) where
import OmegaY.Fam.Dbms.Up2DbmsCore (expandWith)
expandText :: String -> Int -> String
expandText t i = expandWith False False t i False
expandAlter :: String -> Int -> String
expandAlter t i = expandWith False False t i True
