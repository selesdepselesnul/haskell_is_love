module Tree
    (
    insert
    , visitPreorder
    , visitInorder
    , visitPostorder
    ) where

data Tree a = EmptyTree | Tree {item :: a, getLeftChild :: Tree a,
    getRightChild :: Tree a} deriving(Show)

insert :: (Ord a)
       => a
       -> Tree a
       -> Tree a
insert x EmptyTree = Tree x EmptyTree EmptyTree
insert x xxs@(Tree y leftChild rightChild)
    |  x > y = Tree y leftChild $ makeTree rightChild
    |  x < y = Tree y (makeTree leftChild) rightChild
    |  otherwise = xxs
    where makeTree = insert x

visitPreorder :: (Ord a)
              => Tree a
              -> [a]
visitPreorder EmptyTree = []
visitPreorder (Tree y EmptyTree EmptyTree) = [y]
visitPreorder (Tree y leftChild rightChild) =  [y]
                                               ++ visitPreorder leftChild
                                               ++ visitPreorder rightChild

visitInorder :: (Ord a)
             => Tree a
             -> [a]
visitInorder EmptyTree = []
visitInorder (Tree x EmptyTree EmptyTree) = [x]
visitInorder (Tree x leftChild rightChild) = visitInorder leftChild
                                             ++ [x]
                                             ++ visitInorder rightChild


visitPostorder :: (Ord a)
               => Tree a
               -> [a]
visitPostorder EmptyTree = []
visitPostorder (Tree x EmptyTree EmptyTree) = [x]
visitPostorder (Tree x leftChild rightChild) = visitPreorder leftChild
                                               ++ visitPostorder rightChild
                                               ++ [x]
