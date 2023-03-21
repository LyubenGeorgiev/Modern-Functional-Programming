-- Task 1
-- Remove duplicates from a list and maintain original order of elements
setof :: (Eq a) => [a] -> [a]
setof [] = []
setof (x:xs) = x : (setof $ filter (/=x) xs)
 
-- Task 2
-- Given an element X and list of elements return list of 
-- lists with elements representing every possible insertion of the element X
insertat :: a -> [a] -> [[a]]
insertat x [] = [[x]]
insertat x (y:ys) = (x:y:rest) : (map (y:) whole)
    where whole@((_:rest):_) = insertat x ys
 
-- Task 3
-- Given a list return list of lists representing 
-- every permutation of the elements in the original list
perms :: [a] -> [[a]]
perms [] = []
perms [a] = [[a]]
perms (x:xs) = concat $ map (insertat x) (perms xs)
 
-- Task 4
-- Given a list return list of lists representing every subset of the original list
sets :: [a] -> [[a]]
sets [] = [[]]
sets (x:xs) = res ++ map (x:) res
    where res = sets xs