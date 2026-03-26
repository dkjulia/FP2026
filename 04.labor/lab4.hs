import Data.List
import Data.Ord

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- az első n páros szám négyzetét,
parosNegyzet n = [x ** 2 | x <- [2, 4 .. n * 2]]

parosNegyzet2 n = take n [i ^ 2 | i <- [2, 4 ..]]

-- az első [ 1 , 2 , 2 , 3 , 3 , 3 , 4 , 4 , 4 , 4 , … ] ,

szamokLs n
  | n /= 1 = szamokLs (n - 1) ++ replicate n n
  | otherwise = replicate n n

szamokLs2 n i
  | i /= n = replicate i i ++ szamokLs2 n (i + 1)
  | otherwise = replicate i i

szamokLs3 n i
  | i /= n = replicate i (i * 2) ++ szamokLs3 n (i + 1)
  | otherwise = replicate i (i * 2)

szamokLs4 n = [n, n - 1 .. 1] ++ [1 .. n]

szamokLs5 n = reverse [1 .. n] ++ [1 .. n]

fel2 n = take n (ls 1)
  where
    aux i = ls i ++ aux (i + 1)
    ls i = replicate i i : ls (i + 1)

fel2_2 n = aux n
  where
    aux 1 = replicate 1 1
    aux i = aux (i - 1) ++ replicate i i

-- az első [ 2 , 4 , 4 , 6 , 6 , 6 , 8 , 8 , 8 , 8 … ] ,

fel3 n = aux n
  where
    aux 1 = replicate 1 1
    aux i = aux (i - 1) ++ replicate i (i * 2)

-- fel3_2 n id
--   |i /=n = replicate i (i*2)

-- az első [ n , n − 1 , … , 2 , 1 , 1 , 2 , … , n − 1 , n ] ,

fel4 n = [n, n - 1 .. 1] ++ [1 .. n]

-- váltakozva tartalmazzon True és False értékeket,

fel5 n = take n ls
  where
    ls = [True, False] ++ ls

-- váltakozva tartalmazza a 0 ,   1 ,   − 1 értékeket.

fel6 n = take n ls
  where
    ls = [0, 1, -1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- meghatározza egy adott szám osztóinak számát,

osztok1 n = foldl (\res x -> if mod n x == 0 then res + 1 else res) 0 [1 .. n]

osztok2 n = length [i | i <- [1 .. n], mod n i == 0]

osztok3 n = foldl (\res x -> if mod n x == 0 then res + 1 else res) 1 [1 .. div n 2]

osztok n = [i | i <- [1 .. n], mod n i == 0]

osztokSzama n = length $ osztok n

osztokSz2 x = myLength [i | i <- [1 .. x], mod x i == 0]
  where
    myLength [] = 0
    myLength (_ : ls) = 1 + myLength ls

-- meghatározza egy adott szám legnagyobb páratlan osztóját,

maxParatlanOszto n = last $ filter odd $ osztok n

maxParatlanOsztok n = maximum [i | i <- [1 .. n], mod n i == 0, odd i]

maxParatlanOsztok2 n = last [i | i <- [1 .. n], mod n i == 0, odd i]

-- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
-- meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
-- meghatározza az a és b közötti Fibonacci számokat, a > 50 .

maxParatlanOsztok5 n
  | odd n = n
  | otherwise = foldl (\acc x -> if mod n x == 0 then x else acc) 1 [1, 3 .. n]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decP x p
  | x < p = [x]
  | otherwise = decP (div x p) p ++ [mod x p]

decPSzam x p = length $ decP x p

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
decPMax x p = maximum $ decP x p

decPMax2 x p = myMaximum $ decP x p
    where
        myMaximum[n]= n
        myMaximum(n1:n2:ls)
            |n1>n2 = myMaximum(n1:ls)
            |otherwise = myMaximum(n2:ls)

-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.
fibo a b = dropWhile (< a) $ fiboSg 0 1 0
  where
    fiboSg a1 b1 res
      | res < b = res : fiboSg b1 res (res + b1)
      | otherwise = [res]

fibo2 = fiboSg 0 1 0
  where
    fiboSg a b res = res : fiboSg b res (b + res)


fiboAB a b = dropWhile (< a) $ takeWhile (< b) fibo2

fiboAB2 a b = (dropWhile (< a) . takeWhile (< b)) fibo2

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- meghatározza egy lista pozitív elemeinek átlagát,

atlag ls = sum ls / fromIntegral(length ls)

pozAtlag ls = atlag [i| i<-ls, i>0]

pozAtlag2 ls = (atlag . filter (>0)) ls



-- meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,

listaN ls n = [i |(idx, i)<-zip[1 ..] ls, mod idx n == 0]


listaN2 ls n i
  | i >= length ls = []
  | mod i n == 0 = ls !! i : listaN2 ls n (i + 1)
  | otherwise = listaN2 ls n (i + 1)

--  tükrözi egy lista elemeit,
-- két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be, 
-- meghatározza egy lista leggyakrabban előforduló elemét.


listaN3 ls n = map snd (filter (\x -> mod (fst x) n == 0) (zip [1 ..] ls))

-- - tükrözi egy lista elemeit,
tukroz ls = reverse ls

tukroz2 ls = map (\x -> read x :: Int) $ map (reverse . show) ls

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl (\acc d -> acc * 10 + d) 0

tukor3 ls = map (digitsToNumber . tukorSzam) ls
  where
    tukorSzam x
      | x < 10 = [x]
      | otherwise = mod x 10 : tukorSzam (div x 10)

-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
maxElemPoz ls = [idx | (idx, i) <- zip [1 ..] ls, i == myMax]
  where
    myMax = maximum ls

maxElemPoz2 (x : ls) = foldl aux (x, [1]) (zip ls [2 ..])
  where
    aux (currentMax, positions) (elem, i)
      | elem > currentMax = (elem, [i])
      | elem == currentMax = (elem, positions ++ [i])
      | otherwise = (currentMax, positions)

maxElemI ls = ids
  where
    maxElem = maximum ls
    ids = [i | (i, elem) <- zip [1 ..] ls, elem == maxElem]

maxElemI2 ls =
  let maxElem = maximum ls
      ids = [i | (i, elem) <- zip [1 ..] ls, elem == maxElem]
   in ids

maxElemI3 [] = []
maxElemI3 (x : xs) = reverse positions
  where
    (_, positions) = foldl update (x, [0]) (zip xs [1 ..])
    update (currentMax, positions) (elem, idx)
      | elem > currentMax = (elem, [idx])
      | elem == currentMax = (currentMax, idx : positions)
      | otherwise = (currentMax, positions)

maxElemI4 ls = getMaxi 1 maxElem ls []
  where
    maxElem = maximum ls
    getMaxi _ _ [] res = res
    getMaxi i maxe (k : ve) res
      | k == maxe = getMaxi (i + 1) maxe ve (res ++ [i])
      | otherwise = getMaxi (i + 1) maxe ve res

-- - meghatározza egy lista leggyakrabban előforduló elemét.
elof ls = maxElofElem
  where
    maxElofSzam = maximum $ map length $ (group . sort) ls
    ls2 = map (\x -> (head x, length x)) $ (group . sort) ls
    maxElofElem = filter (\x -> snd x == maxElofSzam) ls2

leggyakoribb [] = []
leggyakoribb ls = k : leggyakoribb ve
  where
    k = (kurrens, length [e | e <- ls, e == kurrens])
    ve = [e | e <- ls, e /= kurrens]
    kurrens = head ls

leggyakoribb2 [] = error "ures lista"
leggyakoribb2 ls = head $ maximumBy (comparing length) $ group $ sort ls

leggyakoribb3 ls = head lgyE
  where
    elof = leggyakoribb ls
    lgyE = sortOn (Down . snd) elof