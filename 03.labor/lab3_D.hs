import Data.Foldable

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

myLength2 ls = foldr (\x db -> 1 + db) 0 ls

myLength5 ls = foldl (\db x -> 1 + db) 0 ls

myLength3 xs = foldr (\_ -> (+) 1) 0 xs

ls1 = [[1, 2, 3], [1 .. 10]]

ls2 = [[], [1, 2, 3]]

myLengthMap = map myLength ls2

-- meghivas: myLength4 [1..10] 0
myLength4 [] res = res
myLength4 (x : xs) res = myLength4 xs (res + 1)

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- meghivas: myProduct2 [1..10] 1
myProduct2 [] res = res
myProduct2 (x : xs) res = myProduct2 xs (res * x)

myProduct3 ls = foldr (*) 1 ls

myProduct4 ls = foldl (*) 1 ls

myProduct5 ls = foldr1 (*) ls

myProduct6 ls = foldl1 (*) ls

myProduct7 ls = foldl' (*) 1 ls

myProduct8 ls = foldr' (*) 1 ls

myProductMap ls = map myProduct ls

-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum [x] = x
myMinimum (x1 : x2 : xs) = if x1 < x2 then myMinimum (x1 : xs) else myMinimum (x2 : xs)

myMinimum2 [x] = x
myMinimum2 (x1 : x2 : xs)
  | x1 < x2 = myMinimum2 (x1 : xs)
  | otherwise = myMinimum2 (x2 : xs)

myMinimum3 ls = minimum ls

myMinimum4 ls = foldr1 min ls

myMinimumMap ls = map myMinimum ls

-- - meghatározza egy lista legnagyobb elemét (myMaximum),
myMaximum [] = error "ures lista"
myMaximum [x] = x
myMaximum (x1 : x2 : xs) = if x1 > x2 then myMaximum (x1 : xs) else myMaximum (x2 : xs)

myMaximum2 [x] = x
myMaximum2 (x1 : x2 : xs)
  | x1 > x2 = myMaximum2 (x1 : xs)
  | otherwise = myMaximum2 (x2 : xs)

myMaximum3 ls = foldl1 max ls

myMaximum4 [x] = x
myMaximum4 (x : xs) = max x (myMaximum4 xs)

myMaximumMap = map myMaximum ls1

-- - meghatározza egy lista n-ik elemét (!!),
listaN ls n = ls !! n

listaN2 ls n
  | ls == [] = error "ures lista"
  | length ls <= n = error "tul nagy index"
  | n < 0 = error "negativ index"
  | otherwise = ls !! n

ls3 = [([1 .. 10], 0), ([5, 6, 7], 2)]

listaNMap = map (uncurry listaN) ls3

listaNMap2 = map (\x -> listaN x 0) ls1

-- - egymásután fűzi a paraméterként megadott két listát (++),
listaFuz ls1 ls2 = ls1 ++ ls2

listaFuz2 ls1 ls2 = (++) ls1 ls2

ls4 = [[1 .. 10], [6 .. 15]]

ls5 = [[1, 2, 3], [6, 7, 8]]

listaFuzMap = map (uncurry listaFuz) (zip ls4 ls5)

-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
palindrom ls = if ls == reverse ls then "palindrom" else "nem palindrom"

palindrom2 [] = True
palindrom2 [x] = True
palindrom2 ls = (head ls == last ls) && palindrom2 (init $ tail ls)

ls6 = [[1, 2, 1], [1, 2, 3]]

palindromMap = map palindrom ls6

-- - meghatározza egy egész szám számjegyeinek listáját,
szjLs x
  | x < 10 = [x]
  | otherwise = szjLs (div x 10) ++ [mod x 10]

szjLs2 x
  | x < 10 = [x]
  | otherwise = (mod x 10) : szjLs2 (div x 10)

szjLs2sg x = reverse (szjLs2 x)

-- - a lista első elemét elköltözteti a lista végére,
elsoUtolso (x : xs) = xs ++ [x]

elsoUtolso2 xs = tail xs ++ [head xs]

-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
lsAtlag ls = osszeg / hossz
  where
    osszeg = sum ls
    hossz = fromIntegral (length ls)

-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
decP x p
  | x < p = [x]
  | otherwise = decP (div x p) p ++ [mod x p]

-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.
pDec ls p = foldl (\sg x -> sg * p + x) 0 ls

pDec2 x p = [i * (p ^ hatvany) | (i, hatvany) <- zip (szamjegyek x p) [0 ..]]
  where
    szamjegyek x p
      | x < 10 = [x]
      | otherwise = mod x 10 : szamjegyek (div x 10) p

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.


aLs=[3,-2,5,-7]

x0=2
poli [] x0=0
poli (a: aLs) x0= a+x0*(poli aLs)

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
type Pont =(Double,Double)

lsP :: [Pont]
lsP = [(2.5,5.6),(1.2, 4.5),(6,7)]

p::Pont
p=(3.6,8.9)

tavolsag (x1,y1) (x2,y2) = sqrt((x1-x2)**2+(y1-y2)**2)
 
minPont lsP p = foldl1 aux lsP
 where
  aux p1 p2 = if tavolsag p1 p <tavolsag p2 p then p1 else p2 

minPont2 lsP p=minimumBy(compare 'on' tavolsag p) lsP