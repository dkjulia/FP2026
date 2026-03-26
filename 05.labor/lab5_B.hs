import Data.Foldable

-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.
splitAt' idx ls = (take idx ls, drop idx ls)

splitAt2 idx ls = aux idx ls []
  where
    aux idx ls result
      | idx <= 0 = (reverse result, ls)
      | null ls = (reverse result, ls)
      | otherwise = aux (idx - 1) (tail ls) (head ls : result)

notElem' :: (Foldable t, Eq a) => a -> t a -> Bool
notElem' n = not . elem n

compose2 :: (a -> b -> c) -> (c -> d) -> a -> b -> d
compose2 f g x y = g (f x y)

notElem2 :: (Foldable t, Eq a) => a -> t a -> Bool
notElem2 = compose2 elem not

notElem3 _ [] = True
notElem3 n (x : ls)
  | n == x = False
  | otherwise = notElem3 n ls

concat' lss = foldl' (++) [] lss

repeat' n = n : repeat' n

takeRepeat x n = take x $ repeat' n

replicate' x n = take x $ repeat' n

cycle' ls = ls ++ cycle' ls

takeCycle x ls = take x $ cycle' ls

iterate' fg n = n : iterate' fg (fg n)

takeIterate x fg n = take x $ iterate' fg n

any' _ [] = False
any' fg (x : ls)
  | fg x = True
  | otherwise = any' fg ls

any2 fg ls = not $ null $ filter fg ls

any3 fg ls = foldl (\res x -> fg x || res) False ls

all' fg ls = compose2 any not (not . fg) ls

all2 fg [] = True
all2 fg (x : ls)
  | fg x = all2 fg ls
  | otherwise = False

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
-- - meghatározza egy lista pozitív elemeinek összegét,
-- - egy lista páros elemeinek szorzatát,
-- - n-ig a négyzetszámokat.
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
