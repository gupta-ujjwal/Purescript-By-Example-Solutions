module Main where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)

type Entry = {
  firstName :: String,
  lastName :: String,
  address :: Address
}

type Address = {
  street :: String,
  city :: String,
  state :: String
}

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.firstName <> ", " <> entry.lastName <> ", " <> showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street <> ", " <> address.city <> ", " <> address.state 

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook ->AddressBook
-- insertEntry entry book = Cons entry book
insertEntry = Cons --By using eta conversion converted the finction to point free form

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry fname lname book = head $ filter filterEntry book 
  where 
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == fname && entry.lastName == lname


removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy cond book
  where
    cond a b = a.firstName == b.firstName && a.lastName == b.lastName
    