module Test.MySolutions where

import Prelude

import Data.Array (cons, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Formatter.Internal (repeat)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over2, wrap)
import Data.Show.Generic (genericShow)

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary})
    | imaginary < 0.0 = show real <> show imaginary <> "i"
    | otherwise = show real <> "+" <> show imaginary <> "i"

derive newtype instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _
-- 왜 derive newtype instance 가 아닌거지?

instance semiringComplex :: Semiring Complex where
  zero = wrap zero
  one = wrap one
  add = over2 Complex (+)
  mul = over2 Complex
    \ {real: r1, imaginary: i1} {real: r2, imaginary: i2}
    -> {real: r1 * r2 - i1 * i2, imaginary: r1 * i2 + r2 * i1}
-- 복소수의 곱 (a,b) * (c,d) = (ac - bd, ad + bc)
-- Semiring의 instance를 만드는데, Semiring의 함수를 사용할 수 있다??
-- Newtype을 derive했기 때문에, wrap zero = Complex { real: 0.0, imaginary: 0.0}이 된다?

instance ringComplex :: Ring Complex where
  sub = over2 Complex (-)
-- semiring은 zero, one, add, mul
-- ring은 sub

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty a b) = show a <> show b
-- show (NonEmpty a (Array a)) = show a <> show b
-- 에러 메세지 Unknown data constructor Array
-- 왜 이렇게는 안되는거지?

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a c) (NonEmpty b d) = eq a b && eq c d

instance semigroupNonEmpty :: Semigroup (Array a) => Semigroup (NonEmpty a) where
  append (NonEmpty a c) (NonEmpty b d) = NonEmpty a (c <> cons b d)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a b) = NonEmpty (f a) (map f b)

data Extended a = Infinite | Finite a

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite _ = false
  eq _ Infinite = false
  eq (Finite a) (Finite b) = eq a b

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite b) = compare a b
-- 꼭 Eq의 instance도 만들어야 하나??
-- Ord a의 constraints를 주고 compare a b를 하면 Eq instance 없이도 되어야 하는 것 아닌가?

instance foldableNonEmpty :: Foldable Array => Foldable NonEmpty where
  foldr f z (NonEmpty a b) = foldr f z ([a] <> b)
  foldl f z (NonEmpty a b) = foldl f z ([a] <> b)
  foldMap f (NonEmpty a b) = foldMap f ([a] <> b)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl f z (OneMore a b) = foldl f (f z a) b
  foldr f z (OneMore a b) = f a (foldr f z b)
  foldMap f (OneMore a b) = f a <> foldMap f b -- 모노이드는 결합법칙이 성립된다.

derive newtype instance eqPoint :: Eq Point
-- instance eqPoint :: Eq Point where
--   eq (Point {x: ax, y: ay}) (Point {x: bx, y: by}) = (eq ax bx) && (eq ay by)
-- 이것은 매직인가??
-- derive (newtype) instance, newtype가 있어도 없어도 빌드가 된다..?

derive instance eqShape :: Eq Shape
-- instance eqShape :: Eq Shape where
--   eq (Circle pointA a) (Circle pointB b) = (eq pointA pointB) && (eq a b) 
--   eq (Rectangle pointA a1 a2) (Rectangle pointB b1 b2) = (eq pointA pointB) && (eq a1 b1) && (eq a2 b2)
--   eq (Line pointA1 pointA2) (Line pointB1 pointB2) = (eq pointA1 pointB1) && (eq pointB1 pointB2)
--   eq (Text pointA stringA) (Text pointB stringB) = (eq pointA pointB) && (eq stringA stringB)
--   eq _ _ = false
-- 이것은 매직인가2??

dedupShapes :: Eq Shape => Array Shape -> Array Shape
dedupShapes = nubEq

derive newtype instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapesFast :: Eq Shape => Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs =
  case maximum xs of
    Just m -> m

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive newtype instance showMultiply :: Show Multiply
derive newtype instance eqMultiply :: Eq Multiply

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act m x = case m <> (Multiply x) of
    Multiply y -> y
  -- act (Multiply m) x = m * x
  -- 도 가능하긴 한데, 위의 것이 더 적합하지 않을까?
  -- 왜냐하면 Semigroup, Monoid 인스턴스를 만들었으니까

instance actionMultiplyString :: Action Multiply String where
  act (Multiply m) x = repeat x m
-- Data.Monoid power도 가능

instance actionArray :: Action m a => Action m (Array a) where
  act m xs = act m <$> xs

newtype Self m = Self m

derive newtype instance showSelf :: Show m => Show (Self m)
derive newtype instance eqSelf :: Eq m => Eq (Self m)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

-- functional dependency가 뭐지? https://book.purescript.org/chapter6.html#exercises-3 6번

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates xs = not $ eq xs xs'
  where
    xs' = nubByEq (\a b -> (hashEqual a b) && ((==) a b)) xs
-- hasEqual로 비교할 때는 [1, 2, 3]이 되지만, (==)로 비교할 때는 [65536, 1, 2, 3]이 된다.

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour hr) = hash (hr `mod` 12)
