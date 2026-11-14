{-# LANGUAGE OverloadedStrings #-}
-- | UP2DBMS v1C（id: up2dbms-v1c）。
module OmegaY.Fam.Dbms.Up2V1c (expandText, expandAlter) where
import OmegaY.Fam.Dbms.Up2DbmsCore (expandWith)
expandText :: String -> Int -> String
expandText t i = expandWith False True t i False
expandAlter :: String -> Int -> String
expandAlter t i = expandWith False True t i True
