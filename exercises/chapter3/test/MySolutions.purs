module Test.MySolutions where

import Prelude
import Data.AddressBook (AddressBook, Entry)
import Data.Maybe (Maybe(..))
import Data.List (List(..), filter, head, null, nubByEq)
import Data.HeytingAlgebra (not)

{-- 1. head :: AddressBook -> Maybe Entry
    2. filterEntry :: Entry -> Boolean
    3. filter :: (Entry -> Boolean) -> AddressBook -> AddressBook
--}

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry = ((==) street) <<< _.address.street
-- eta conversion
-- backward/forward composition

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not <<< null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = (entry.firstName == firstName) && (entry.lastName == lastName)
-- eta conversion을 할 수 있는 방법은 없을까?

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq nameEq
  where
    nameEq :: Entry -> Entry -> Boolean
    nameEq a b = (a.firstName == b.firstName) && (a.lastName == b.lastName)
-- eta conversion을 할 수 있는 방법은 없을까?