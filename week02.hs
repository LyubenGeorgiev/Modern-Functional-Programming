import Data.Char
import Data.List

-- Task 1
-- Group consecutive elements satisfying the predicat
-- Return list of pairs (length of the chain, first element)
-- [3,5,3,3,0,-2,8,8]
groupCount :: (a -> a -> Bool) -> [a] -> [(Int, a)]
groupCount _ [] = []
groupCount _ [a] = [(1,a)]
groupCount pred (x:xs) = if pred x y then (cnt+1,x):ys else (1,x):res
  where
    res@((cnt,y):ys) = groupCount pred xs


-- Task 2
-- Given a string representing math Expression return a string visually representing it's AST
-- Input contains only:
--  Variables: english character
--  Math operations (ordered by priority):
--      +
--      -
--      *
--      /
--      ^ (power)
--  Brackets:
--      (
--      )

splitAtFirst :: (a -> Bool) -> [a] -> ([a],a,[a])
splitAtFirst _ [] = undefined
splitAtFirst p (x:xs)
 | p x = ([], x, xs)
 | otherwise = (x:ys, r, zs)
    where
      (ys, r, zs) = splitAtFirst p xs

priority ')' = 0
priority '+' = 1
priority '-' = 1
priority '*' = 2
priority '/' = 2
priority '^' = 3
priority _ = 4

transform :: String -> IO ()
transform expr = putStr $ snd $ format 0 $ reverse $ toPolishN (reverse expr) [] []

format :: Int -> String -> (Int, String)
format _ [] = (0,[])
format spaces (x:xs)
 | isAlpha x = (1, [' ' | i <- [1..spaces]] ++ (x:"\n"))
 | otherwise = (1 + used1 + used2, [' ' | i <- [1..spaces]] ++ (x:"\n") ++ res1 ++ res2)
    where
        (used1, res1) = format (spaces + 2) xs
        (used2, res2) = format (spaces + 2) $ drop used1 xs


toPolishN [] stack res = res ++ stack
toPolishN ('(':xs) stack res = toPolishN xs stackRemain (res ++ stackRemove)
  where
    (stackRemove, _, stackRemain) = splitAtFirst (==')') stack
toPolishN (')':xs) stack res = toPolishN xs (')':stack) res
toPolishN (token:cs) stack res
 | isAlpha token = toPolishN cs stack (res ++ [token])
 | otherwise = toPolishN cs (token : stackDrop) (res ++ stackTake)
    where
        stackTake = takeWhile ((>(priority token)) . priority) stack
        stackDrop = dropWhile ((>(priority token)) . priority) stack