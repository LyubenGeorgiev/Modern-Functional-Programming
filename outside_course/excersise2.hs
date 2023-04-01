data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving (Show)

-- Insert an element in an Ordered Binary Tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert val Nil = Node Nil val Nil
insert val (Node l v r)
 | val <= v = Node (insert val l) v r
 | otherwise = Node l v (insert val r)

-- Given a tree return the inorder traversal
inorder :: Tree a -> [a]
inorder Nil = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r