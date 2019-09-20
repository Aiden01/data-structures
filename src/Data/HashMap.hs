{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.HashMap
  ( empty
  , exists
  , replace
  , insert
  , get
  , singleton
  )
where

import           Data.Hashable
import           Common

newtype Bucket k v = Bucket [(k, v)] deriving Show
newtype HashMap k v = HashMap [Bucket k v] deriving Show

getBucket :: Hashable k => k -> HashMap k v -> Int
getBucket _ (HashMap []     ) = 0
getBucket k (HashMap buckets) = hash k `mod` length buckets

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex idx x = zipWith (\i x' -> if i == idx then x else x') [0 ..]

exists :: (Eq k, Hashable k) => k -> HashMap k v -> Bool
exists _ (HashMap []) = False
exists key hashMap@(HashMap buckets) =
  let (Bucket entries) = buckets !! getBucket key hashMap
  in  any (\(k, _) -> k == key) entries

replace :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
replace _ _ (HashMap []) = HashMap []
replace key value hashMap@(HashMap buckets)
  | not $ exists key hashMap
  = hashMap
  | otherwise
  = let
      idx              = getBucket key hashMap
      (Bucket entries) = buckets !! idx
      newBucket =
        Bucket $ map (\x@(k, _) -> if k == key then (k, value) else x) entries
    in
      HashMap (replaceAtIndex idx newBucket buckets)


insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert key value hashMap@(HashMap buckets)
  | exists key hashMap
  = replace key value hashMap
  | null buckets
  = HashMap [Bucket [(key, value)]]
  | otherwise
  = let idx              = getBucket key hashMap
        (Bucket entries) = buckets !! idx
        newBucket        = Bucket (entries ++ [(key, value)])
    in  HashMap (replaceAtIndex idx newBucket buckets)

get :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
get _ (HashMap []) = Nothing
get key hashMap@(HashMap buckets) =
  let idx              = getBucket key hashMap
      (Bucket entries) = buckets !! idx
  in  lookup key entries

delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete key hashMap@(HashMap buckets) =
  let idx              = getBucket key hashMap
      (Bucket entries) = buckets !! idx
      newBucket        = Bucket $ filter ((== key) . fst) entries
  in  HashMap (replaceAtIndex idx newBucket buckets)

empty :: HashMap k v
empty = HashMap []

singleton :: k -> v -> HashMap k v
singleton k v = HashMap [Bucket [(k, v)]]

instance Foldable (Bucket k) where
  foldr f acc (Bucket []      ) = acc
  foldr f acc (Bucket (x : xs)) = foldr f (f (snd x) acc) (Bucket xs)

instance Foldable (HashMap k) where
  foldr f acc (HashMap []      ) = acc
  foldr f acc (HashMap (x : xs)) = foldr f (foldr f acc x) (HashMap xs)

instance (Eq k, Hashable k) => From (HashMap k v) [(k, v)] where
  from = foldl (\set (k, v) -> insert k v set) empty


