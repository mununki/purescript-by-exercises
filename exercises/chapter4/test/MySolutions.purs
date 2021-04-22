module Test.MySolutions where

import Data.Maybe
import Prelude
import Test.Examples

import Control.MonadZero (guard)
import Data.Array (cons, filter, head, init, last, length, tail, (..))
import Data.Foldable (foldl, maximumBy, minimumBy)
import Data.Int (pow)
import Data.Path (Path(..), filename, isDirectory, size)
import Data.String (joinWith, split)
import Data.String.Pattern (Pattern(..))

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

countEven :: Array Int -> Int
countEven [] = 0
countEven xs =
  case isEven <$> head xs of
    Just true | Just xs' <- (tail xs) -> 1 + countEven xs'
    otherwise | Just xs' <- (tail xs) -> countEven xs'
    otherwise -> countEven []

squared :: Array Number -> Array Number
squared = map (\n -> n * n)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\n -> n >= 0.0)
-- Int가 Number 하위클래스 쯤 될 거라고 생각했는데, Array Int는 타입 에러가 남.

infix 5 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = (<$?>) (\n -> n >= 0.0)
-- keepNonNegativeRewrite = (\n -> n >= 0.0) <$?> 얜 왜 안되지? eta conversion 때문? -> 괄호를 써서 함수로 만들면 된다!
-- infixl, infixr, infix 에 따라 무슨 차이지?
{--
왜 이런 식으로는 안되는거지?
Unexpected token '(<$?>)'

(<$?>) :: forall a. (a -> Boolean) -> Array Number -> Array Number
(<$?>) = filter
--}

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = (==) 1 $ length $ factors n

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  x <- a
  y <- b
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ isPythagoreanTriples a b c
  pure [a, b, c]
    where isPythagoreanTriples a b c = ((pow a 2) + (pow b 2)) == (pow c 2)

factorize :: Int -> Array Int
factorize n = primes n []
  where
    primes :: Int -> Array Int -> Array Int
    primes 1 xs = reverse xs
    primes x xs = case isPrime x of
      true | mod n x == 0 -> primes (x - 1) (cons x xs)
      otherwise -> primes (x - 1) xs

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- (Medium - No Test) Characterize those arrays xs for which the function foldl (==) false xs returns true. In other words, complete the sentence: "The function returns true when xs contains ..."
-- (ANSWER) all false

fibTailRec :: Int -> Int
fibTailRec n = calculator n 0 1
  where
    calculator :: Int -> Int -> Int -> Int
    calculator 0 n2 n1 = n2 + n1
    calculator i n2 n1 = calculator (i - 1) (n2 + n1) n2

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc x -> (cons x acc)) []

onlyFiles :: Path -> Array Path
onlyFiles file = filter (\x -> not $ isDirectory x) $ allFiles file

-- 나 자신을 뺀 하위 Path들
allFilesExceptSelf :: Path -> Array Path
allFilesExceptSelf file = filter (\x -> not $ (filename x) == (filename file)) $ allFiles file

whereIs :: Path -> String -> Maybe Path
whereIs file target = head $ scanner file
  where
    scanner :: Path -> Array Path
    scanner file' = do
      child <- onlyFiles file'
      guard $ eq target $ fromMaybe "" $ last $ split (Pattern "/") $ filename child
      pure (File (joinWith ""
        $ reverse 
        $ cons "/"
        $ reverse
        $ cons "/"
        $ fromMaybe []
        $ init
        $ split (Pattern "/")
        $ filename child) 0)

-- 하위 디렉토리의 모든 파일들의 크기의 합을 구하는 함수
totalSize :: Path -> Int
totalSize file = foldl (\acc x -> (+) acc $ fromMaybe 0 $ (size x)) 0 $ allFiles file

largestSmallest :: Path -> Array Path
largestSmallest (File _ _) = []
largestSmallest (Directory _ []) = []
largestSmallest (Directory _ [(File n s)]) = [(File n s)]
largestSmallest file = [l, s]
  where
    l = fromMaybe (File "" 0) $ maximumBy (\a b -> comparing size a b) $ onlyFiles file
    s = fromMaybe (File "" 0) $ minimumBy (\a b -> comparing size a b) $ onlyFiles file
