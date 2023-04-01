data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving (Show)


-- Gives Infinite tree
inf_tup_tree :: Tree (Integer, Integer)
inf_tup_tree = aux (0, 0)
  where
    aux (l, r) = Node (aux $ (l + 1, r)) (l, r) (aux $ (l, r + 1))

-- Gives only the first K levels of the tree
cut :: Integer -> Tree a -> Tree a
cut 0 _ = Nil
cut _ Nil = Nil
cut n (Node l v r) = Node (cut (n-1) l) v (cut (n-1) r)