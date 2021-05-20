module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Person (Person)
import Data.Picture (Bounds, Picture, Point, Shape(..), bounds, getCenter, intersect, origin)
import Data.Picture as Data.Picture
import Math (pi)

factorial :: Int -> Int
factorial n = compute n 1
  where
  compute 0 acc = acc
  compute n' acc = compute (n' - 1) (acc * n')

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
binomial n k | otherwise = nF / (kF * nMinusKF)
  where
  nF = factorial n
  kF = factorial k
  nMinusKF = factorial (n - k)

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = (pascal (n - 1) k) + (pascal (n - 1) (k - 1))

-- row polymorphism 기본적으로 작동하지 않는다.
sameCity :: Person -> Person -> Boolean
sameCity p1 p2 = p1.address.city == p2.address.city

sameCity2 :: Person -> Person -> Boolean
sameCity2 {address: {city: p1City}} {address: {city: p2City}} = p1City == p2City

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [y] = y
fromSingleton x _ = x

circleAtOrigin :: Shape
circleAtOrigin = Circle {x: 0.0, y: 0.0} 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter shape = case shape of
  (Circle p r) -> (Circle origin (r * 2.0))
  (Rectangle p w h) -> (Rectangle origin (w * 2.0) (h * 2.0))
  (Line s e) | center <- getCenter shape -> (Line ((s - center) * {x: 2.0, y: 2.0}) ((e - center) * {x: 2.0, y: 2.0}))
-- getCenter (Line s e) = (s + e) * {x: 0.5, y: 0.5} 레코드의 합과 곱?!
-- 대수적 데이터 타입이어서 가능하다?
  (Text loc str) -> (Text origin str)

shapeText :: Shape -> Maybe String
shapeText (Text loc str) = Just str
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = (Watt (a * v))

area :: Shape -> Number
area (Circle p r) = pi * r * r
area (Rectangle p w h) = w * h
area _ = 0.0

data Shape2 = Clipped Picture Point Number Number

shapeBounds :: Shape2 -> Bounds
shapeBounds (Clipped p pt w h) = intersect (bounds p) (Data.Picture.shapeBounds (Rectangle pt w h))
-- 문제가 잘 이해가 안되서.. 요건 no-peeking을 훔쳐봤습니다 😃

-- 숙제

-- 리스크립트 sum type, product type 은 뭔가?
-- 리스크립트 row polymorphism이 잘 구현되어있는 자료구조는 무엇인가?
-- 결국 이 둘이 합쳐져서 row polymorphism 이 잘 구현되어있는 자료 구조 와 아닌 것
-- -> 각각 sum type, product type 자료구조 찾기

-- type constructor와 data constructor 정리하기

-- 숙제 : https://www.notion.so/5-6e2d667f5c514554a969b1e74c2a8fd5