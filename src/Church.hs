module Church where

_0 f x = x
_1 f x = f x
_2 f x = f $ f x

_true x y = x
_false x y = y

_inc n f x = f $ n f x

unchurchNum n = n (+ 1) 0

unchurchBool b = b True False
