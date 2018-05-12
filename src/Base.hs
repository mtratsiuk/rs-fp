{-# LANGUAGE NoImplicitPrelude #-}

module Base where

import Prelude (undefined, ($), Show, Int, Num, (+), (*))

identity :: a -> a
identity x = x

constant :: a -> b -> a
constant x _ = x

apply :: (a -> b) -> a -> b
apply f x = f x

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f $ g x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

ap :: (a -> b -> c) -> (a -> b) -> a -> c
ap f g x = f x (g x)

data Maybe a
  = Nothing
  | Just a
  deriving (Show)
