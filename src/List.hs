{-# LANGUAGE NoImplicitPrelude #-}

module List where

import Prelude (undefined, Show, Eq, Ord, Int, Num, Bool, Enum, (+), (*), (-), (>), (==), succ, otherwise)
import Base

data List a
  = Empty
  | Cons a (List a)
  deriving (Show, Eq)


foldl :: (b -> a -> b) -> b -> List a -> b
foldl f acc Empty = acc
foldl f acc (Cons x xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f acc (Cons x Empty) = f x acc
foldr f acc (Cons x xs) = f x (foldr f acc xs)

prepend :: a -> List a -> List a
prepend = Cons

append :: a -> List a -> List a
append x = foldr Cons (Cons x Empty)

sum :: (Num a) => List a -> a
sum = foldl (+) 0

product :: (Num a) => List a -> a
product = foldl (*) 1

length :: List a -> Int
length = foldl (\a c -> a + 1) 0

reverse :: List a -> List a
reverse = foldl (flip prepend) Empty

map :: (a -> b) -> List a -> List b
map f = foldr (compose Cons f) Empty

filter :: (a -> Bool) -> List a -> List a
filter pred = foldr (\c a -> if (pred c) then Cons c a else a) Empty

concat :: List (List a) -> List a
concat = foldr (flip (foldr Cons)) Empty

range :: (Ord a, Enum a) => a -> a -> List a
range l r
  | l > r     = Empty
  | l == r    = Cons l Empty
  | otherwise = Cons l (range (succ l) r)

zeroTo :: Int -> List Int
zeroTo = range 0

repeat :: Int -> a -> List a
repeat 0 = constant Empty
repeat n = ap Cons (repeat (n - 1))

concatMap :: (a -> List b) -> List a -> List b
concatMap f = compose concat (map f)
