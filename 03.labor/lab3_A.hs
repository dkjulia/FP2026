-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

-- meghivas myLength2 [1..100] 0
myLength2 [] res = res
myLength2 (x : xs) res = myLength2 xs (res + 1)

myLength3 ls = foldr (\x db -> (+) 1 db) 0 ls

myLength4 ls = foldl (\db x -> (+) 1 db) 0 ls

myLength5 ls res = foldr (\x res -> 1 + res) res ls

myLength6 ls = length ls

ls1 = [[1, 2, 3], [4, 5]]

myLengthLs = map myLength ls1

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myProduct2 [] res = res
myProduct2 (x : xs) res = myProduct2 xs (res * x)

myProduct3 ls = foldr1 (*) ls

myProduct4 ls = product ls

-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum [] = error "ures lista"
myMinimum [x] = x
myMinimum (x1 : x2 : xs)
  | x1 < x2 = myMinimum (x1 : xs)
  | otherwise = myMinimum (x2 : xs)

myMinimum2 [] = error "ures lista"
myMinimum2 [x] = x
myMinimum2 (x1 : x2 : xs) =
  if x1 < x2
    then myMinimum2 (x1 : xs)
    else myMinimum2 (x2 : xs)

myMinimum3 ls = foldl1 min ls

myMinimum4 ls = minimum ls

-- - meghatározza egy lista legnagyobb elemét (myMaximum),
myMaximum [] = error "ures lista"
myMaximum [x] = x
myMaximum (x1 : x2 : xs)
  | x1 > x2 = myMaximum (x1 : xs)
  | otherwise = myMaximum (x2 : xs)

myMaximum2 ls = foldr1 max ls

myMaximum3 ls = maximum ls

-- - meghatározza egy lista n-ik elemét (!!),
listaN ls n = ls !! n

listaN2 ls n
  | ls == [] = error "ures lista"
  | n < 0 = error "neg. index"
  | length ls <= n = error "tul nagy index"
  | otherwise = ls !! n

listaNMap ls = map (\x -> listaN x 0) ls

ls2 = [([1, 2, 3], 0), ([1 .. 10], 5)]

listaNMap2 = map (uncurry listaN) ls2

-- - egymásután fűzi a paraméterként megadott két listát (++),
lsFuz ls1 ls2 = ls1 ++ ls2

ls3 = [[1 .. 10], [5, 66]]

ls4 = [[23, 56, 7], [213, 56], [67, 32]]

lsFuzMap = map (uncurry lsFuz) (zip ls3 ls4)

-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
palindrom ls = ls == reverse ls

palindrom2 ls = if ls == reverse ls then "palindrom" else "nem palindrom"

palindrom3 [] = True
palindrom3 [x] = True
palindrom3 ls = head ls == last ls && palindrom3 ((init . tail) ls)

-- - meghatározza egy egész szám számjegyeinek listáját,
szjLs x
  | x < 0 = szjLs (abs x)
  | x < 10 = [x]
  | otherwise = szjLs (div x 10) ++ [mod x 10]

-- - a lista első elemét elköltözteti a lista végére,
elsoUtolso ls = tail ls ++ [head ls]

elsoUtolso2 (x : xs) = xs ++ [x]

-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
decP x p
  | x < 0 = error "neg. szam"
  | x < p = [x]
  | otherwise = decP (div x p) p ++ [mod x p]

-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.
pDec ls p = foldl (\hatvany x -> x + (p ** hatvany)) 0 ls

pDec2 x p =
  let szamjegyek x
        | x < 10 = [x]
        | otherwise = mod x 10 : szamjegyek (div x 10)
      szjIdx = zip (szamjegyek x) [0 ..]
   in sum [i * (p ^ hatvany) | (i, hatvany) <- szjIdx]

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.
aLs = [3, -2, 5, -7]

x0 = 2

poli [] x = 0
poli (a : aLs) x = a + x * (poli aLs x)

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
type Pont = (Double, Double)

lsP :: [Pont]
lsP = [(4.5, 6.2), (1.2, 3.4), (6, 8), (4.5, 2.4), (4.5, 6.1)]

p :: Pont
p = (3.4, 5.6)

tavolsag (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

minTavolsag lsP p = foldl1 (\p1 p2 -> if tavolsag p1 p < tavolsag p2 p then p1 else p2) lsP

minTavolsag2 lsP p = foldl1 aux lsP
  where
    aux p1 p2 = if tavolsag p1 p < tavolsag p2 p then p1 else p2

minTavolsag3 [p1] _ = p1
minTavolsag3 (p1 : p2 : ls) p
  | tavolsag p1 p < tavolsag p2 p = minTavolsag3 (p1 : ls) p
  | otherwise = minTavolsag3 (p2 : ls) p