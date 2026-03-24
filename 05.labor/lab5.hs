import Distribution.Simple.Setup (falseArg)
import System.Posix.Internals (lstat)

splitAt' idx ls = (idxElotti, idxUtani)
  where
    idxElotti = take idx ls
    idxUtani = drop idx ls

notElem' e (k : ls)
  | null ls = True
  | e == k = False
  | otherwise = notElem' e ls

elem' e (k : ls)
  | null ls = False
  | e == k = True
  | otherwise = elem' e ls

concat' lss = foldl1 (++) lss

repeat' n = n : repeat' n

takeRepeat' elemSzam n = take elemSzam $ repeat' n

replicate' n e
  | n > 1 = e : replicate' (n - 1) e
  | otherwise = e : replicate' (n - 1) e

eplicate2 n e
  | n == 1 = [e]
  | otherwise = e : replicate' (n - 1) e

cycle' ls = ls ++ cycle' ls

takeCycle n ls = take n $ cycle' ls

takeCycle2 n ls = take n (cycle' ls)

takeCycle3 n ls = (take n . cycle') ls

takeCycle4 n ls = take n . cycle' $ ls

iterate' fg e = fg e : iterate' fg (fg e)

takeIterate n fg e = take n $ iterate' fg e

any' feltetel (x : ls)
  | null ls = False
  | feltetel x = True
  | otherwise = any' feltetel ls

all' feltetel (x : ls)
  | null ls = True
  | not (feltetel x) = False
  | otherwise = all' feltetel ls

length2 :: (Foldable t, Num b) => t a -> b
length2 ls = foldr (\x res -> res + 1) 0 ls

sum' ls = foldl (+) 0 ls

elem2 e ls = foldl (\res x -> if x == 0 then True else res) False ls

reverse' ls = foldl (\res x -> x : res) [] ls

reverse2 ls = foldr (\x res -> res ++ [x]) [] ls

product' ls = foldl (*) 1 ls

product2 ls = foldl1 (*) ls

maximum' ls = foldl1 (\res x -> if x > res then x else res) ls

insertSort [] = []
insertSort (x : xs) = insert x (insertSort xs)
  where
    insert y [] = [y]
    insert y (z : zs) = if y >= x then y : z : zs else z : insert y zs

insertSort' ls = foldr insert [] ls
  where
    insert x [] = [x]
    insert x (y : ys) = if x <= y then x : y : ys else y : insert x ys

listaFuz lss = foldl (++) [] lss

map' fg ls = foldl (\res x -> res ++ (fg x)) [] ls

map2 fg ls = foldr (\x res -> fg x : res) [] ls

filter' feltetel ls = foldl (\res x -> if feltetel x then res ++ [x] else res) [] ls

pozSum ls = foldl (\res x -> if x > 0 then res + x else res) 0 ls

parosSzorzat ls = foldl (\res x -> if even x then res * x else res) 1 ls

negyzetSzamokN n = foldl (\res x -> res ++ x ^ 2) [] [1 .. n]