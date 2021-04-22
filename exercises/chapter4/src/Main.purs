module Main where

import Prelude

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  logShow $ isSameList isSame (1 : 2 : 3 : Nil) ("1" : "2" : "3" : Nil)

isSame :: Int -> String -> Boolean
isSame n s = eq s $ show n

isSameList :: (Int -> String -> Boolean) -> List Int -> List String -> List Boolean
isSameList f ln ls = isSame <$> ln <*> ls