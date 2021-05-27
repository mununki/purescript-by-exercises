module Test.MySolutions where

import Prelude

import Data.Array (foldM, head, nub, sort, tail)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (error, throwException)

-- into a larger programming language supporting non-deterministic choice
-- effect =? non-deterministic

-- Applicative
-- class (Apply f) <= Applicative f where
--   pure :: forall a. a -> f a

-- Apply
-- class (Functor f) <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b

-- Bind
-- class (Apply m) <= Bind m where
--   bind :: forall a b. m a -> (a -> m b) -> m b

-- Monad
-- class (Applicative m, Bind m) <= Monad m where
-- 왜 return 같은 녀석을 구현을 안한거지? pure가 있으니 필요 없어서?

-- Functor -- Apply -- Applicative -- Monad
--                   \    Bind     /

----------------
-- Exercises #1
----------------
-- 1
third :: forall a. Array a -> Maybe a
third xs = head =<< tail =<< tail xs
-- 동일
-- third arr = do
--   rest1 <- tail arr
--   rest2 <- tail rest1
--   third' <- head rest2
--   pure third'

-- 2
possibleSums :: Array Int -> Array Int
possibleSums xs = sort $ nub $ foldM (\z a -> [z, z + a]) 0 xs
-- 이것은 모나드의 매직인가??
-- 모든 조합의 최소공배수를 구할 수 있다!
-- possibleMuls :: Array Int -> Array Int
-- possibleMuls arr = sort $ nub $ foldM (\acc a -> [acc, acc * a]) 1 arr

-- 3
-- Applicative는 Monad의 슈퍼클래스이고, Apply는 Applicative의 슈퍼클래스여서,
-- Monad를 구현한 Maybe도 apply를 사용할 수 있다. ap는 Monad에 구현되어있음.

-- 4
-- Maybe가 Apply, Applicative, Bind를 구현하고 있어서, Monad의 법칙을 만족한다.

-- 5
filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  x' <- f x
  xs' <- filterM f xs
  pure if x' then x : xs' else xs'

-- 6
-- 모나드는 Fuctor, Apply, Applicative, Bind를 갖고 있으니 다 된다.

----------------
-- Exercises #2
----------------
-- 1
safeDivide :: Int -> Int -> Effect Int
safeDivide _ 0 = throwException $ error "div zero"
safeDivide a b = pure (a / b)

-- ST는 생략 😃