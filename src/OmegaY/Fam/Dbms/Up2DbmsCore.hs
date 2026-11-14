{-# LANGUAGE OverloadedStrings #-}
-- | up2dbms v1 / v1b+ / v1c 共用外壳。差异仅在 v1flag / v1c。
module OmegaY.Fam.Dbms.Up2DbmsCore (expandWith) where

import OmegaY.Fam.Dbms.Common
import OmegaY.Fam.Dbms.MnUp2

isLimitWord :: String -> Bool
isLimitWord s = s == "Limit" || s == "limit" || s == "LIMIT"

expandWith :: Bool -> Bool -> String -> Int -> Bool -> String
expandWith v1flag v1c text term longer =
  let term' = if term < 1 then 1 else term
   in if isLimitWord text
        then displayExpr (infinityFs term')
        else case parseExpr text of
          Nothing -> errParse
          Just expr ->
            if null expr
              then ""
              else
                if not (exprValid expr)
                  then errParse
                  else
                    if null (last expr)
                      then displayExpr (init expr)
                      else displayExpr (dbmsExpand expr term' (not longer) v1flag v1c)
