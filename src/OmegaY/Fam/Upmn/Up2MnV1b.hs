{-# LANGUAGE OverloadedStrings #-}
-- | up2mn-v1b / up2mn-v1b-plus。移植 up2mn_v1b.hpp / up2mn_v1b_plus.hpp。
module OmegaY.Fam.Upmn.Up2MnV1b (expandText, expandAlter) where

import OmegaY.Fam.Dbms.Common
import OmegaY.Fam.Dbms.MnUp2
  ( parseExpr, displayExpr, exprValid, up2mnExpand )

errP = errParse

expandTextImpl :: String -> Int -> Bool -> Bool -> String
expandTextImpl text term longer plus =
  let term' = if term < 1 then 1 else term
   in case parseExpr text of
            Nothing -> errP
            Just e ->
              if null e then ""
              else if not (exprValid e) then errP
              else if null (last e) then displayExpr (init e)
              else displayExpr (up2mnExpand e term' (not longer) plus)

expandText :: Bool -> String -> Int -> String
expandText plus text term = expandTextImpl text term False plus

expandAlter :: Bool -> String -> Int -> String
expandAlter plus text term = expandTextImpl text term True plus
