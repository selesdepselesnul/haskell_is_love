module List (List(..), append) where
-- reimplement linked list data structure :v

data List a = EmptyList | List {item :: a, next :: List a}
    deriving (Show)

append :: a -> List a -> List a
append item EmptyList = List item EmptyList
append item (List x EmptyList) = List item $ List x EmptyList
append item x = List item x
