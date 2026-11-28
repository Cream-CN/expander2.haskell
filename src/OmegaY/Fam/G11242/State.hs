-- | 11242 家族内部用的小型状态单子（计算预算 + 计数器），只依赖 base。
module OmegaY.Fam.G11242.State
  ( M (..)
  , runM
  , get
  , put
  , modify
  , tick
  ) where

newtype M s a = M {unM :: s -> Either String (a, s)}

instance Functor (M s) where
  fmap f (M g) = M $ \s -> case g s of
    Left e -> Left e
    Right (a, s') -> Right (f a, s')

instance Applicative (M s) where
  pure a = M $ \s -> Right (a, s)
  M f <*> M g = M $ \s -> case f s of
    Left e -> Left e
    Right (h, s') -> case g s' of
      Left e -> Left e
      Right (a, s'') -> Right (h a, s'')

instance Monad (M s) where
  M g >>= f = M $ \s -> case g s of
    Left e -> Left e
    Right (a, s') -> unM (f a) s'

-- | 用初始状态跑完整个计算。
runM :: M s a -> s -> Either String a
runM (M g) s = case g s of
  Left e -> Left e
  Right (a, _) -> Right a

get :: M s s
get = M $ \s -> Right (s, s)

put :: s -> M s ()
put s = M $ \_ -> Right ((), s)

modify :: (s -> s) -> M s ()
modify f = M $ \s -> Right ((), f s)

-- | 工作量计数器加一；超过上限即失败（上限由调用方在状态里判定）。
tick :: M Int ()
tick = M $ \s -> Right ((), s + 1)
