{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Set
  ( Set(..)
  )
where

import           Common

data Set a = Empty | Cons a (Set a) deriving (Show)


add :: Eq a => a -> Set a -> Set a
add x set | x `elem` set = set
          | otherwise    = go x set
 where
  go x Empty        = Cons x Empty
  go x (Cons a set) = Cons a (go x set)

prepend :: Eq a => a -> Set a -> Set a
prepend x set | x `elem` set = set
              | otherwise    = Cons x set

merge :: Eq a => Set a -> Set a -> Set a
merge = go
 where
  go Empty        set  = set
  go (Cons a set) set' = prepend a (go set set')

instance Functor Set where
  fmap f Empty        = Empty
  fmap f (Cons x set) = Cons (f x) (fmap f set)

instance Applicative Set where
  pure x = Cons x Empty
  Empty      <*> _           = Empty
  _          <*> Empty       = Empty
  Cons f set <*> Cons a set' = Cons (f a) (set <*> set')


instance Foldable Set where
  foldr f acc Empty      = acc
  foldr f acc (Cons a x) = foldr f (f a acc) x

instance Eq a => From (Set a) [a] where
  from = foldl (\set x -> if x `elem` set then set else add x set) Empty

