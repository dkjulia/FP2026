-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
-- atlag :: (Floating a) => [a] -> a
-- atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
-- - meghatározza egy lista legkisebb elemét (myMinimum),
-- - meghatározza egy lista legnagyobb elemét (myMaximum),
-- - meghatározza egy lista n-ik elemét (!!),
-- - egymásután fűzi a paraméterként megadott két listát (++),
-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
-- - meghatározza egy egész szám számjegyeinek listáját,
-- - a lista első elemét elköltözteti a lista végére,
-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.
aLs = [3, -2, 5, -7]

x0 = 2

poli [] _ = 0
poli (a : aLs) x = a + x * (poli aLs x)

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
type Pont = (Double, Double)

lsP :: [Pont]
lsP = [(3.4, 2.4), (8.7, 1.2), (4, 5), (1.2, 23.8)]

p :: Pont
p = (3.4, 1.7)

tavolsag (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

minTavolsag lsP p = foldl1 aux lsP
  where
    aux p1 p2 = if tavolsag p1 p < tavolsag p2 p then p1 else p2

minTavolsag3 lsP p = foldl1 (\p1 p2 -> if tavolsag p1 p < tavolsag p2 p then p1 else p2) lsP

minTavolsag2 [] _ = error "ures lista"
minTavolsag2 [p1] _ = p1
minTavolsag2 (p1 : p2 : lsP) p
  | tavolsag p1 p < tavolsag p2 p = minTavolsag2 (p1 : lsP) p
  | otherwise = minTavolsag2 (p2 : lsP) p