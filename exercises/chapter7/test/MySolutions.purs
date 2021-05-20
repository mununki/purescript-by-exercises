module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Data.Validation.Semigroup (V)

----------------
-- Exercises #1
----------------
-- 1
addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (+)

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 (-)

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 (*)

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 (/)

-- 2
addApply :: forall a f. Semiring a => Apply f => f a -> f a -> f a
addApply a b = (+) <$> a <*> b

subApply :: forall a f. Ring a => Apply f => f a -> f a -> f a
subApply a b = (-) <$> a <*> b

mulApply :: forall a f. Semiring a => Apply f => f a -> f a -> f a
mulApply a b = (*) <$> a <*> b

divApply :: forall a f. EuclideanRing a => Apply f => f a -> f a -> f a
divApply a b = (/) <$> a <*> b

-- 3
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe = sequence
-- sequence 👍
-- combineMaybe (Just x) = Just <$> x
-- combineMaybe Nothing = pure Nothing

----------------
-- Exercises #2
----------------
-- 1
stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z][a-zA-Z]$" noFlags

-- 2
nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

-- 3
validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex a.state

----------------
-- Exercises #3
----------------
-- 1
data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)

derive instance genericTree :: Generic (Tree a) _

-- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md
instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

-- 2
instance functorTree :: Functor Tree where
  map f Leaf = Leaf
  map f (Branch l b r) = Branch (map f l) (f b) (map f r)

instance foldableTree :: Foldable Tree where
  foldr f z Leaf = z
  foldr f z (Branch l b r) = foldr f (f b (foldr f z r)) l
  foldl f z Leaf = z
  foldl f z (Branch l b r) = foldl f (f (foldl f z l) b) r
  foldMap f Leaf = mempty -- mempty를 바로 쓸 수 있다!
  foldMap f (Branch l b r) = foldMap f l <> f b <> foldMap f r

instance traversableTree :: Traversable Tree where
  traverse f Leaf = pure Leaf
  traverse f (Branch l b r) = ado
    ml <- traverse f l
    mb <- f b
    mr <- traverse f r
    in Branch ml mb mr
  -- 위와 동일
  -- traverse f (Branch l b r) = Branch <$> traverse f l <*> f b <*> traverse f r
  -- data constructor는 함수이다.
  sequence Leaf = pure Leaf
  sequence (Branch l b r) = ado
    ml <- sequence l
    mb <- b
    mr <- sequence r
    in Branch ml mb mr

-- 3
traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder f Leaf = pure Leaf
traversePreOrder f (Branch l b r) = ado
  mb <- f b
  ml <- traversePreOrder f l
  mr <- traversePreOrder f r
  in Branch ml mb mr

-- 4
traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder f Leaf = pure Leaf
traversePostOrder f (Branch l b r) = ado
  ml <- traversePostOrder f l
  mr <- traversePostOrder f r
  mb <- f b
  in Branch ml mb mr

-- 5
type Person
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

personWithOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> Person
personWithOptionalAddress firstName lastName homeAddress phones = {firstName, lastName, homeAddress, phones}

validatePersonOptionalAddress :: Person -> V Errors Person
validatePersonOptionalAddress p =
  personWithOptionalAddress
    <$> nonEmpty "First Name" p.firstName
    <*> nonEmpty "Last Name" p.lastName
    <*> traverse validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

-- 6
sequenceUsingTraverse :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse = traverse identity

-- 7
traverseUsingSequence :: forall t a b m. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f ta = sequence (f <$> ta)
