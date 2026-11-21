-- | e0MN 与 strong e0MN：基于 mnordshare 的序数记号。
module OmegaY.Fam.Mn.E0Mn
  ( e0MnNotation
  , strongE0MnNotation
  ) where

import OmegaY.Notation.Types (Notation, textAlterNotation)
import OmegaY.Fam.Mn.MnOrdShare

errText :: String
errText = "解析失败，请确认你没有使用全角括号，逗号，分号"

-- | C++ expand_text_impl(text, term, longer)：shorter = !longer。
expandImpl :: Bool -> Bool -> String -> Int -> String
expandImpl strong longer text term0 =
  let term = max 1 term0
   in case parseExpr text of
        Nothing -> errText
        Just pr ->
              let e = pr
               in if null e
                    then ""
                    else if not (isLimitExpr e)
                      then exprToPlain (predecessor e)
                      else exprToPlain (fs e term (not longer) strong)

e0MnNotation :: Notation
e0MnNotation =
  textAlterNotation "e0mn" "e0MN" []
    (expandImpl False False)
    (expandImpl False True)
    ""

strongE0MnNotation :: Notation
strongE0MnNotation =
  textAlterNotation "strong-e0mn" "strong e0MN" []
    (expandImpl True False)
    (expandImpl True True)
    ""
