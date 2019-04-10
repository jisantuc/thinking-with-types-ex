module Lib where

-- Exercise 6.3-i
-- What's the rank of Int -> forall a. a -> a?
-- I propose that it is 1 -- since it's polymorphic but not weird
f :: forall a . a -> a
f x = x

-- Exercise 6.3-ii
-- What is the rank of (a -> b) -> (forall c . c -> a) -> b?
-- (a -> b) -> ((forall c . c -> a) -> b)
-- forall is to the left of... two arrows, so 2
-- correct!

-- Exercise 6.3-iii
-- What is the rank of ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a?
-- 3? I guess?
-- Turns out... correct! Since the forall is to the left of a function arrow... used
-- by two other function arrows I guess, so that's what "every time" means

newtype Cont a = Cont { unCont :: forall r . (a -> r) -> r }

instance Functor Cont where
  fmap f (Cont g) = Cont (\h -> g $ f h)
