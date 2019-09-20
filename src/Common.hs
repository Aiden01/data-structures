{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Common
  ( From(..)
  )
where

class From a b where
  from :: b -> a

instance From a a where
  from = id
