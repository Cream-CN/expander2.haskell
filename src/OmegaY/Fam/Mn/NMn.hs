-- | nMN（1MN/2MN/3MN）：基于 smnshare::expand 的参数化模板。
module OmegaY.Fam.Mn.NMn
  ( nMnNotation
  ) where

import OmegaY.Notation.Types (Notation, textAlterNotation)
import OmegaY.Fam.Mn.SmnShare

errText :: String
errText = "解析失败，请确认你没有使用全角括号，逗号，分号"

-- | C++ NMnNotation<N>::expand_text_impl。
expandImpl :: Int -> Bool -> String -> Int -> String
expandImpl n longer text term0 =
  let term = max 1 term0
   in if text == "Limit"
        then mountainDisplay (ntInfinityFs n term) False
        else case fromDisplay text of
               Nothing -> errText
               Just m
                 | null m -> ""
                 | null (last m) -> mountainDisplay (init m) False
                 | otherwise -> mountainDisplay (expand m term (not longer)) False

nMnNotation :: Int -> String -> String -> Notation
nMnNotation n ident name =
  textAlterNotation ident name []
    (expandImpl n False)
    (expandImpl n True)
    ""
