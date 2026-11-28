-- | 记号接口与统一展开分派，对齐 C++ 侧 @notation/registry.hpp@ 的
--   @INotation::expand@ 语义：每个记号只有一种展开（“保留末项”），
--   没有 FS / FS_alter 双模式，也不做通用截断。
module OmegaY.Notation.Types
  ( Caps (..)
  , Notation (..)
  , seqNotation
  , textNotation
  , textAlterNotation
  , paramNotation
  , paramAlterNotation
  , expandNotation
  , uiExpand
  ) where

import OmegaY.Parse (parseIntStrict, parseSequence, seqToString)

data Caps
  = SeqCaps ([Int] -> Int -> [Int]) String
  | TextCaps (String -> Int -> String) String
  | ParamCaps (String -> Int -> Int -> String) String

data Notation = Notation
  { nId :: String
  , nName :: String
  , nTags :: [String]
  , nParam :: Maybe (Int, Int, Int, String)
      -- ^ (min, max, default, label)
  , nCaps :: Caps
  }

seqNotation :: String -> String -> [String] -> ([Int] -> Int -> [Int]) -> String -> Notation
seqNotation ident name tags expand suffix =
  Notation ident name tags Nothing (SeqCaps expand suffix)

textNotation :: String -> String -> [String] -> (String -> Int -> String) -> String -> Notation
textNotation ident name tags expand suffix =
  Notation ident name tags Nothing (TextCaps expand suffix)

-- | C++ dev 已整体移除 FS_alter：每个记号只剩记号自身实现的“保留末项”展开。
--   老移植里 @expandText@（旧 FS 分支）与 @expandTextAlter@ 两支并存，
--   C++ 侧保留的是后一支（旧 alter 语义），因此这里取 @alter@ 作为唯一展开。
textAlterNotation :: String -> String -> [String] -> (String -> Int -> String)
                  -> (String -> Int -> String) -> String -> Notation
textAlterNotation ident name tags _expand alter suffix =
  Notation ident name tags Nothing (TextCaps alter suffix)

paramNotation :: String -> String -> [String] -> (Int, Int, Int, String)
              -> (String -> Int -> Int -> String) -> String -> Notation
paramNotation ident name tags prm expand suffix =
  Notation ident name tags (Just prm) (ParamCaps expand suffix)

-- | 参数化记号的单展开版本，语义同 'textAlterNotation'。
paramAlterNotation :: String -> String -> [String] -> (Int, Int, Int, String)
                   -> (String -> Int -> Int -> String) -> (String -> Int -> Int -> String)
                   -> String -> Notation
paramAlterNotation ident name tags prm _expand alter suffix =
  Notation ident name tags (Just prm) (ParamCaps alter suffix)

-- | 单一展开入口；错误文本与 C++ 侧逐字一致。
expandNotation :: Notation -> String -> Int -> Int -> Either String String
expandNotation n text term param =
  case nCaps n of
    TextCaps expand suffix -> Right (expand text term ++ suffix)
    ParamCaps expand suffix ->
      case nParam n of
        Just (mn, mx, _, _)
          | param < mn || param > mx ->
              Left
                ( "参数必须是 " ++ show mn ++ " 到 " ++ show mx
                    ++ " 之间的整数"
                )
        _ -> Right (expand text term param ++ suffix)
    SeqCaps expand suffix
      | term < 1 -> Left "项数必须为正整数"
      | otherwise ->
          let seq = parseSequence text
           in if null seq
                then Left "序列不能为空"
                else Right (seqToString (expand seq term) ++ suffix)

-- UI 级校验：逐字复刻 Expander.cpp 的三条分支（文本型 / 参数化 / 序列型）
-- 的提示语与判定顺序。termRaw / paramRaw 是输入框里的**原始文本**，
-- 等价于 C++ 侧 std::stoi 的成败语义。
--
-- 说明：C++ dev 的注册中心已不再有 FS / FS_alter 两个按钮，界面层的这组
-- 前置校验文案仍按原样保留；本仓库的 CLI 不暴露这条路径。

uiErrTermNotInt, uiErrTermNotPos, uiErrTermParamNotInt :: String
uiErrTermNotInt = "项数必须是整数，\n所以说你为什么要这样输入"
uiErrTermNotPos = "项数必须为正整数，\n你难道要尝试向内存反方向写入序列吗"
uiErrTermParamNotInt = "项数与参数都必须是整数，\n所以说你为什么要这样输入"

-- | 返回 @Left 提示语@（UI 弹窗文本）或 @Right 展开结果@。
uiExpand :: Notation -> String -> String -> String -> Either String String
uiExpand n termRaw paramRaw text =
  case nCaps n of
    TextCaps {} ->
      case parseIntStrict termRaw of
        Nothing -> Left uiErrTermNotInt
        Just term
          | term < 1 -> Left uiErrTermNotPos
          | null text -> Left "序列不能为空"
          | otherwise -> expandNotation n text term 0
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
                    else expandNotation n text term param
    SeqCaps {} ->
      case parseIntStrict termRaw of
        Nothing -> Left "项数必须是整数"
        Just term
          | term < 1 -> Left "项数必须为正整数"
          | null (parseSequence text) -> Left "序列不能为空"
          | otherwise -> expandNotation n text term 0
