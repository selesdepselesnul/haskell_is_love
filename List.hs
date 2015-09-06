module List
    (List(..),
     add,
     head'
    ) where

data List a = EmptyList | List {item :: a , next :: List a}
    deriving (Show)

instance Eq (List a) where
    EmptyList == EmptyList = True

instance Foldable List where
    foldr func acc EmptyList = acc
    foldr func acc (List x EmptyList) = func x acc
    foldr func acc (List x n) =  func x $ foldr func acc n

add :: a
    -> List a
    -> List a
add x EmptyList = List x EmptyList
add x xxs@(List _ EmptyList) = List x xxs
add x xs = List x xs

head' :: List a
      -> a
head' EmptyList = error "Empty List you mutha fucka!"
head' (List a _) = a

index :: Int
      -> List a
      -> a
index _ EmptyList = error "you mutha fucka!"
index _ (List x EmptyList) = x
index num (List x n)
    | num == 0 = x
    | otherwise = index (num - 1) n


addList :: List a
        -> List a
        -> List a
addList EmptyList EmptyList = EmptyList
addList (List x EmptyList) yy = List x yy
addList EmptyList x = x
addList (List x n) y = List x $ addList n y


last' :: List a
      -> a
last' EmptyList = error "Error you mutha fucka!"
last' (List x EmptyList) = x
last' (List x n) = last' n

tail' :: List a
      -> List a
tail' EmptyList = error "Error you mutha fucka!"
tail' xxs@(List _ EmptyList) = EmptyList
tail' (List _ xxs@(List _ _)) = xxs

init' :: List a
      -> List a
init' EmptyList = error "Error you mutha fucka!"
init' (List _ EmptyList) = EmptyList
init' (List x n) = addList (List x EmptyList) (init' n)

elem' :: (Eq a)
      => a
      -> List a
      -> Bool
elem' a EmptyList = False
elem' a (List x n) = if a == x then True else elem' a n

map' :: (a -> b)
     -> List a
     -> List b
map' func EmptyList = EmptyList
map' func (List x n) = List (func x) $ map' func n
