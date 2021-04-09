module Test.MySolutions where

import Prelude

import Data.Int (rem, toNumber)
import Math (pi, pow, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = (*) pi $ pow r $ toNumber 2
-- Int는 Number 타입 클래스를 만족하지 않는가?

leftoverCents :: Int -> Int
leftoverCents n = rem n 100