{-# LANGUAGE RankNTypes #-}

module Church where


-- Boolean

type ChurchBool = forall a. a -> a -> a

_true :: ChurchBool
_true x y = x

_false :: ChurchBool
_false x y = y

_and :: ChurchBool -> ChurchBool -> ChurchBool
_and x y = x y _false

_or :: ChurchBool -> ChurchBool -> ChurchBool
_or x y = x _true y

_xor :: ChurchBool -> ChurchBool -> ChurchBool
_xor x y = x (_not y) y

_not :: ChurchBool -> ChurchBool
_not b x y = b y x

_cond :: ChurchBool -> a -> a -> a
_cond b x y = b x y

unchurchBool :: ChurchBool -> Bool
unchurchBool b = b True False


-- Number

newtype ChurchNum = ChurchNum { unwrapNum :: forall a. (a -> a) -> a -> a }

_0 :: ChurchNum
_0 = ChurchNum $ \f x -> x

_1 :: ChurchNum
_1 = ChurchNum $ \f x -> f x

_2 :: ChurchNum
_2 = ChurchNum $ \f x -> f $ f x

_inc :: ChurchNum -> ChurchNum
_inc n = ChurchNum $ \f x -> f $ unwrapNum n f x

_add :: ChurchNum -> ChurchNum -> ChurchNum
_add n m = ChurchNum $ \f x -> unwrapNum n f $ unwrapNum m f x

_mul :: ChurchNum -> ChurchNum -> ChurchNum
_mul n m = ChurchNum $ (unwrapNum n) . (unwrapNum m)

_exp :: ChurchNum -> ChurchNum -> ChurchNum
_exp n m = ChurchNum $ (unwrapNum m) (unwrapNum n)

_dec :: ChurchNum -> ChurchNum
_dec n = _first $ unwrapNum n _slide $ _tuple _0 _0

_sub :: ChurchNum -> ChurchNum -> ChurchNum
_sub n m = unwrapNum m _dec n

_isZero :: ChurchNum -> ChurchBool
_isZero n = unwrapNum n (\_ -> _false) _true

_leq :: ChurchNum -> ChurchNum -> ChurchBool
_leq n m = _isZero $ _sub n m

_geq :: ChurchNum -> ChurchNum -> ChurchBool
_geq n m = _isZero $ _sub m n

_eq :: ChurchNum -> ChurchNum -> ChurchBool
_eq n m = _and gt lt
  where
    lt = _leq n m
    gt = _geq n m

unchurchNum :: ChurchNum -> Int
unchurchNum n = unwrapNum n (+ 1) 0


-- Tuple

type ChurchElem = ChurchNum

newtype ChurchTuple = ChurchTuple { unwrapTuple :: forall a. (ChurchElem -> ChurchElem -> a) -> a }

_tuple :: ChurchElem -> ChurchElem -> ChurchTuple
_tuple l r = ChurchTuple (\f -> f l r)

_first :: ChurchTuple -> ChurchElem
_first t = unwrapTuple t (\l r -> l)

_second :: ChurchTuple -> ChurchElem
_second t = unwrapTuple t (\l r -> r)

_swap :: ChurchTuple -> ChurchTuple
_swap t = unwrapTuple t (\l r -> _tuple r l)

_slide :: ChurchTuple -> ChurchTuple
_slide t = unwrapTuple t (\l r -> _tuple r (_inc r))
