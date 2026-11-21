-- | Aω2MN 族记号（参数化：2/3 变体，weak 弱化分支）。
module OmegaY.Fam.Aw2mn.Aw2mn
  ( aw2mnNotation
  ) where

import OmegaY.Notation.Types (Notation, textAlterNotation)
import OmegaY.Fam.Aw2mn.Aw2mnShare

expandImpl :: Bool -> SubtractKind -> Bool -> String -> Int -> String
expandImpl weak kind longer text term0 =
  let term = max 1 term0
   in case parse text of
        Nothing -> kParseFail
        Just parsed ->
              let e = pExpr parsed
               in if null e
                    then ""
                    else if null (last e)
                      then mountainDisplay (take (length e - 1) e)
                      else if not (validValueRange e)
                        then kParseFail
                        else mountainDisplay (expand e term (not longer) weak kind)

aw2mnNotation :: String -> String -> Bool -> SubtractKind -> Notation
aw2mnNotation ident name weak kind =
  textAlterNotation ident name []
    (expandImpl weak kind False)  -- fs: longer=false → shorter=true
    (expandImpl weak kind True)   -- alter: longer=true → shorter=false
    ""
