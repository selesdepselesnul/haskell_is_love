module Stack
    (
    ) where

push :: (Ord a)
     => a
     -> [a]
     -> [a]
push a [] = [a]
push a xxxs = xxxs ++ [a]

pop :: (Ord a)
    => [a]
    -> (a, [a])
pop [] = error "Stack is Empty"
pop xxxs = (last xxxs, init xxxs)

peek :: [a]
     -> (a, [a])
peek [] = error "Stack is Empty"
peek xxxs = (last xxxs, xxxs)
