{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Common
  ( From(..)
  , Into(..)
  )
where

class From a b where
  from :: b -> a

instance From a a where
  from = id

class Into a b where
  into :: a -> b

instance Into a a where
  into = id
