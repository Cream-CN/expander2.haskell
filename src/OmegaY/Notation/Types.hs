-- | 记号接口与统一展开分派，对齐 C++ 侧 @notation/registry.hpp@ 的
--   @INotation::expand@ 语义（含 FS 通用截断与参数校验错误文本）。
module OmegaY.Notation.Types
  ( ExpandMode (..)
  , Caps (..)
  , Notation (..)
  , seqNotation
  , textNotation
  , textAlterNotation
  , paramNotation
  , paramAlterNotation
  , expandNotation
  , uiExpand
  ) where

import OmegaY.Mrss (mrssTruncate)
import OmegaY.Parse (parseIntStrict, parseSequence, seqToString)

data ExpandMode = Fs | FsAlter
  deriving (Eq, Show)

data Caps
  = SeqCaps ([Int] -> Int -> [Int]) String
  | TextCaps (String -> Int -> String) (Maybe (String -> Int -> String)) String
  | ParamCaps (String -> Int -> Int -> String) (Maybe (String -> Int -> Int -> String)) String

data Notation = Notation
  { nId :: String
  , nName :: String
  , nTags :: [String]
  , nParam :: Maybe (Int, Int, Int, String)
      -- ^ (min, max, default, label)
  , nCaps :: Caps
  , nTruncate :: String -> Int -> String
      -- ^ FS 通用截断策略，默认 MrSS 末元素裁剪
  }

seqNotation :: String -> String -> [String] -> ([Int] -> Int -> [Int]) -> String -> Notation
seqNotation ident name tags expand suffix =
  Notation ident name tags Nothing (SeqCaps expand suffix) mrssTruncate

textNotation :: String -> String -> [String] -> (String -> Int -> String) -> String -> Notation
textNotation ident name tags expand suffix =
  Notation ident name tags Nothing (TextCaps expand Nothing suffix) mrssTruncate

textAlterNotation :: String -> String -> [String] -> (String -> Int -> String)
                  -> (String -> Int -> String) -> String -> Notation
textAlterNotation ident name tags expand alter suffix =
  Notation ident name tags Nothing (TextCaps expand (Just alter) suffix) mrssTruncate

paramNotation :: String -> String -> [String] -> (Int, Int, Int, String)
              -> (String -> Int -> Int -> String) -> String -> Notation
paramNotation ident name tags prm expand suffix =
  Notation ident name tags (Just prm) (ParamCaps expand Nothing suffix) mrssTruncate

paramAlterNotation :: String -> String -> [String] -> (Int, Int, Int, String)
                   -> (String -> Int -> Int -> String) -> (String -> Int -> Int -> String)
                   -> String -> Notation
paramAlterNotation ident name tags prm expand alter suffix =
  Notation ident name tags (Just prm) (ParamCaps expand (Just alter) suffix) mrssTruncate

-- | 单一展开入口；错误文本与 C++ 侧逐字一致。
expandNotation :: Notation -> ExpandMode -> String -> Int -> Int -> Either String String
expandNotation n mode text term param =
  case nCaps n of
    TextCaps expand alter suffix ->
      case (mode, alter) of
        (FsAlter, Just a) -> Right (a text term ++ suffix)
        (FsAlter, Nothing) -> Right (expand text term ++ suffix)
        (Fs, Nothing) ->
          let out = expand text term
           in Right (nTruncate n out term ++ suffix)
        (Fs, Just _) -> Right (expand text term ++ suffix)
    ParamCaps expand alter suffix ->
      case nParam n of
        Just (mn, mx, _, _)
          | param < mn || param > mx ->
              Left
                ( "参数必须是 " ++ show mn ++ " 到 " ++ show mx
                    ++ " 之间的整数"
                )
        _ ->
          case (mode, alter) of
            (FsAlter, Just a) -> Right (a text term param ++ suffix)
            _ -> Right (expand text term param ++ suffix)
    SeqCaps expand suffix
      | term < 1 -> Left "项数必须为正整数"
      | otherwise ->
          let seq = parseSequence text
           in if null seq
                then Left "序列不能为空"
                else
                  let res0 = expand seq term
                      res = if mode == Fs && length res0 > 1 then init res0 else res0
                   in Right (seqToString res ++ suffix)

-- UI 级校验：逐字复刻 Expander.cpp 中 IDM_FS / IDM_FSALTER 的三条分支
-- （文本型 / 参数化 / 序列型）的提示语与判定顺序。termRaw / paramRaw 是
-- 输入框里的**原始文本**，等价于 C++ 侧 std::stoi 的成败语义。

uiErrTermNotInt, uiErrTermNotPos, uiErrTermParamNotInt :: String
uiErrTermNotInt = "项数必须是整数，\n所以说你为什么要这样输入"
uiErrTermNotPos = "项数必须为正整数，\n你难道要尝试向内存反方向写入序列吗"
uiErrTermParamNotInt = "项数与参数都必须是整数，\n所以说你为什么要这样输入"

-- | 返回 @Left 提示语@（UI 弹窗文本）或 @Right 展开结果@。
uiExpand :: Notation -> ExpandMode -> String -> String -> String -> Either String String
uiExpand n mode termRaw paramRaw text =
  case nCaps n of
    TextCaps {} ->
      case parseIntStrict termRaw of
        Nothing -> Left uiErrTermNotInt
        Just term
          | term < 1 -> Left uiErrTermNotPos
          | null text -> Left "序列不能为空"
          | otherwise -> expandNotation n mode text term 0
    ParamCaps {} ->
      case (parseIntStrict termRaw, parseIntStrict paramRaw) of
        (Nothing, _) -> Left uiErrTermParamNotInt
        (_, Nothing) -> Left uiErrTermParamNotInt
        (Just term, Just param)
          | term < 1 -> Left uiErrTermNotPos
          | otherwise ->
              case nParam n of
                Just (mn, mx, _, _)
                  | param < mn || param > mx ->
                      Left
                        ( "参数必须是 " ++ show mn ++ " 到 " ++ show mx
                            ++ " 之间的整数"
                        )
                _ ->
                  if null text
                    then Left "序列不能为空"
                    else expandNotation n mode text term param
    SeqCaps {} ->
      case parseIntStrict termRaw of
        Nothing -> Left "项数必须是整数"
        Just term
          | term < 1 -> Left "项数必须为正整数"
          | null (parseSequence text) -> Left "序列不能为空"
          | otherwise -> expandNotation n mode text term 0
