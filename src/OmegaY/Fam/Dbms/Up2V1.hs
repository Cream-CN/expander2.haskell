{-# LANGUAGE OverloadedStrings #-}
-- | UP2DBMS v1（id: up2dbms-v1）。
module OmegaY.Fam.Dbms.Up2V1 (expandText, expandAlter) where
import OmegaY.Fam.Dbms.Up2DbmsCore (expandWith)
expandText :: String -> Int -> String
expandText t i = expandWith True False t i False
expandAlter :: String -> Int -> String
expandAlter t i = expandWith True False t i True
