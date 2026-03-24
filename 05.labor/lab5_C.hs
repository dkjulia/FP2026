-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.
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

takeRepeat elemSzam n = take elemSzam $ repeat' n

replicate' n e
  | n > 1 = e : replicate' (n - 1) e
  | otherwise = [e]

replicate2 n e
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

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
length' ls = foldl (\res x -> res + 1) 0 ls

length2 ls = foldr (\x res -> res + 1) 0 ls

sum' ls = foldl (+) 0 ls

elem2 e ls = foldl (\res x -> if x == e then True else res) False ls

reverse' ls = foldl (\res x -> x : res) [] ls

reverse2 ls = foldr (\x res -> res ++ [x]) [] ls

product' ls = foldl (*) 1 ls

product2 ls = foldl1 (*) ls

maximum' ls = foldl1 (\res x -> if x > res then x else res) ls

insertSort [] = []
insertSort (x : xs) = insert x (insertSort xs)
  where
    insert y [] = [y]
    insert y (z : zs) = if y <= z then y : z : zs else z : insert y zs

insertSort' ls = foldr insert [] ls
  where
    insert x [] = [x]
    insert x (y : ys) = if x <= y then x : y : ys else y : insert x ys

-- ++
listakFuz lss = foldl (++) [] lss

map' fg ls = foldl (\res x -> res ++ [fg x]) [] ls

map2 fg ls = foldr (\x res -> fg x : res) [] ls

filter' feltetel ls = foldl (\res x -> if feltetel x then res ++ [x] else res) [] ls

-- - meghatározza egy lista pozitív elemeinek összegét,
pozSum ls = foldl (\res x -> if x > 0 then res + x else res) 0 ls

-- - egy lista páros elemeinek szorzatát,
parosSzorzat ls = foldl (\res x -> if even x then res * x else res) 1 ls

-- - n-ig a négyzetszámokat.
negyzetSzamokN n = foldl (\res x -> res ++ [x ^ 2]) [] [1 .. n]

-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
