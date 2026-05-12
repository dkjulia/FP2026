module Lab8 where
import           Data.Function (on)
import           Data.List     (find, groupBy, intercalate, maximumBy, sortBy,
                                sortOn)
import           Data.Ord
import           System.IO

-- # 8. labor
-- I. Írjunk egy Haskell programot, amelyben megadunk egy konstans Fesztivalok elemtípusú listát, majd

data Fesztivalok = Fesztivalok
  { fEgyuttes  :: String,
    fFesztival :: String,
    fAr        :: Int,
    fKod       :: Int
  }
  deriving (Show)

fesztivalok :: [Fesztivalok]
fesztivalok =
  [ Fesztivalok "Coldplay" "Glastonbury" 5000 101,
    Fesztivalok "Radiohead" "Coachella" 4500 102,
    Fesztivalok "Arctic Monkeys" "Glastonbury" 6000 103,
    Fesztivalok "Foo Fighters" "Lollapalooza" 4000 104,
    Fesztivalok "The Killers" "Coachella" 5500 105,
    Fesztivalok "Muse" "Lollapalooza" 3000 106,
    Fesztivalok "Imagine Dragons" "Glastonbury" 7000 107,
    Fesztivalok "Tame Impala" "Coachella" 3500 108,
    Fesztivalok "Red Hot Chili Peppers" "Lollapalooza" 8000 109,
    Fesztivalok "The Strokes" "Glastonbury" 2500 110
  ]

-- 1. határozzuk meg egy adott fesztiválon szereplő eggyütteseket,
fesztEgyuttesek :: [Fesztivalok] -> String -> Maybe (String, [String])
fesztEgyuttesek ls fesztival =
  let rendezett = sortOn fFesztival ls
      csoportositott = groupBy ((==) `on` fFesztival) rendezett
      fE = map (\cs -> (fFesztival (head cs), map fEgyuttes cs)) csoportositott
   in find (\cs -> fst cs == fesztival) fE

-- 2. határozzuk meg azokat az együtteseket, amelyek egy adott értéknél olcsóbban árulják koncertjegyeiket,
olcsobbJegyek :: Int -> Maybe [String]
olcsobbJegyek ar = if null egyuttesek then Nothing else Just egyuttesek
  where
    egyuttesek = map fEgyuttes $ filter (\f -> fAr f < ar) fesztivalok

-- 3. határozzuk meg, hogy hány olyan együttes szerepel a listában, amely egy adott értéknél olcsóbban árulja koncertjegyét,
olcsobbJegyekDb :: Int -> Maybe Int
olcsobbJegyekDb ar = if egyuttesekDb == 0 then Nothing else Just egyuttesekDb
  where
    egyuttesekDb = length $ filter (\f -> fAr f < ar) fesztivalok
-- 4. rendezzük a lista tartalmát az együttesek nevei alapján ábécé sorrendbe (insertSort),
insertSort :: (a -> a -> Bool) -> [a] -> [a]
insertSort cmpFunc [] = []
insertSort cmpFunc (x:xs) = insert cmpFunc x (insertSort cmpFunc xs)
  where
    insert cmpFunc' y [] = [y]
    insert cmpFunc' y (z:zs) = if cmpFunc' y z then y:z:zs else z : insert cmpFunc' y zs

rendezEgyuttesekNev :: [Fesztivalok]
rendezEgyuttesekNev = insertSort (\f1 f2 -> fEgyuttes f1 < fEgyuttes f2) fesztivalok
rendezEgyuttesekNev2 = sortOn fEgyuttes fesztivalok
rendezEgyuttesekNev3 = sortBy (comparing fEgyuttes) fesztivalok

-- 5. rendezzük a lista tartalmát a jegyárak szerint csökkenő sorrendbe (qSort),
quickS :: (a -> a -> Bool) -> [a] -> [a]
quickS cmpFunc [] = []
quickS cmpFunc (k : ve) = quickS cmpFunc kLs ++ [k] ++ quickS cmpFunc nLs
  where
    kLs = [x | x <- ve, cmpFunc x k]
    nLs = [x | x <- ve, not (cmpFunc x k)]

rendezJegyarCsokk :: [Fesztivalok]
rendezJegyarCsokk = quickS (\f1 f2 -> fAr f1 > fAr f2) fesztivalok
rendezJegyarCsokk2 = sortOn (Down . fAr) fesztivalok
rendezJegyarCsokk3 = reverse $ sortOn fAr fesztivalok

-- 6. rendezzük a lista tartalmát összefésülő rendezéssel a kod értékek alapján,
merge :: (t -> t -> Bool) -> [t] -> [t] -> [t]
merge cmp [] ls = ls
merge cmp ls [] = ls
merge cmp ls1@(k1 : ve1) ls2@(k2 : ve2)
  | cmp k1 k2 = k1 : merge cmp ve1 ls2
  | otherwise = k2 : merge cmp ls1 ve2

mergeS :: (a -> a -> Bool) -> [a] -> [a]
mergeS cmp [] = []
mergeS cmp [k] = [k]
mergeS cmp ls = merge cmp bLista jLista
  where
    db = div (length ls) 2
    bLista = mergeS cmp (take db ls)
    jLista = mergeS cmp (drop db ls)

rendezKod = mergeS (\f1 f2 -> fKod f1 < fKod f2) fesztivalok
-- 7. határozzuk, meg, hogy egy adott fesztiválon mennyi a jegyek átlagértéke,
jegyekAtlagFeszt fesztival =
  let
    arak = map fAr $ filter (\f -> fFesztival f == fesztival) fesztivalok
    atlagAr = if null arak then 0 else fromIntegral (sum arak) / fromIntegral (length arak)
  in atlagAr
-- 8. írjuk meg az általános összefésülő, illetve beszúró rendezés algoritmusokat.
merge2 :: (Ord a) => [a]-> [a]-> [a]
merge2 [] ls = ls
merge2 ls [] = ls
merge2 ls1@(k1 : ve1) ls2@(k2 : ve2)
  | k1 < k2 = k1 : merge2 ve1 ls2
  | otherwise = k2 : merge2 ls1 ve2

mergeS2 :: (Ord a) => [a]-> [a]
mergeS2 [] = []
mergeS2 [k] = [k]
mergeS2 ls = merge2 bLista jLista
  where
    db = div (length ls) 2
    bLista = mergeS2 (take db ls)
    jLista = mergeS2 (drop db ls)

insertSort2 :: Ord a => [a] -> [a]
insertSort2 [] = []
insertSort2 (x:xs) = insert x (insertSort2 xs)
  where
    insert y []     = [y]
    insert y (z:zs) = if y <= z then y:z:zs else z : insert y zs

mainI = do
  -- 1.
  let fesztival = "Glastonbury"
      fesztivalEgyuttesek = fesztEgyuttesek fesztivalok fesztival
  case fesztivalEgyuttesek of
    Nothing -> putStrLn "Nincs ilyen fesztival"
    Just egyuttesek -> do
                putStrLn (fesztival ++ " egyuttesei:")
                mapM_ putStrLn (snd egyuttesek)
  -- 2.
  let ar = 4000
      olcsobbJegyEgyuttesek = olcsobbJegyek ar
  case olcsobbJegyEgyuttesek of
    Nothing -> putStrLn ("Nincsenek " ++ show ar ++ " arnal olcsobb jegyek")
    Just egyuttesek -> do
                putStr (show ar ++ " arnal olcsobb egyuttesek: ")
                putStrLn $ intercalate ", " egyuttesek
  let olcsobbJegyEgyuttesekDb = olcsobbJegyekDb ar
  -- 3.
  case olcsobbJegyEgyuttesekDb of
    Nothing -> putStrLn ("Nincsenek " ++ show ar ++ " arnal olcsobb jegyek")
    Just egyuttesekDb -> do
                putStrLn (show ar ++ " arnal olcsobb egyuttesek szama " ++ show egyuttesekDb)
  -- 4.
  let rendezENevLs = rendezEgyuttesekNev
      eNevek = map fEgyuttes rendezENevLs
  putStrLn $ intercalate ", " eNevek
  -- 5.
  let rendezJegyarCsokkLs = rendezJegyarCsokk
      eNevAr = map (\f -> (fEgyuttes f, fAr f)) rendezJegyarCsokkLs
  mapM_ (\(nev, ar) -> putStrLn (nev ++ " " ++ show ar)) eNevAr
  -- 6.
  let rendezKodLs = rendezKod
      eNevKod = map (\f -> (fEgyuttes f, fKod f)) rendezKodLs
  mapM_ (\(nev, kod) -> putStrLn (nev ++ " " ++ show kod)) eNevKod
  -- 7.
  let jegyekAtlagF = jegyekAtlagFeszt fesztival
  putStrLn (fesztival ++ " jegyeinek atlagerteke " ++ show jegyekAtlagF)
  -- 8.


-- II. Egy szövegállományban egy adott városról a következő adatok vannak eltárolva: városnév, népességszám, területméret, azaz adott a következő adatszerkezet:
data Varos = Varos {
  vNev      :: String,
  vNepSzam  :: Int,
  vTerMeret :: Int
} deriving (Show)

-- Írjunk egy Haskell programot, amely az állományban levő adatok alapján létrehoz egy Varos elemtípusú listát, majd
split :: Char -> String -> [String]
split _ [] = [[]]
split delim (c : cs)
  | c == delim = [] : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split delim cs

toVaros line = Varos nev (read nepesseg) (read terMeret)
  where
    [nev, nepesseg, terMeret] = split ',' line

toVaros2 line =
  let (nev, rest1) = break (== ',') line
      (nepStr, rest2) = break (== ',') (tail rest1)
      terStr = tail rest2
   in Varos nev (read nepStr) (read terStr)

-- 1. meghatározza, hogy hány olyan város van, amelyiknek a népsűrűsége (népsűrűség = népesség-szám / terület-méret) egy megadott $[a, b]$ intervallumba esik, ahol az $a$ és $b$ értékeket a billentyűzetről olvassuk be,
varosNepsuruseg varosok = [(vNev v, fromIntegral (vNepSzam v) / fromIntegral (vTerMeret v)) | v <- varosok]
nepsurusegABFg a b varosok
        | a < b = filter (\v -> snd v < b && snd v > a) (varosNepsuruseg varosok)
        | otherwise = filter (\v -> snd v < a && snd v > b) (varosNepsuruseg varosok)

-- 2. meghatározza a városok népsűrűség szerinti rendezett sorrendjét, az eredményt elegáns formában kiírva a képernyőre (népsűrűség = népesség-szám / terület-méret),
negyelemuMasodik (_,x,_,_) = x
rendezNepsuruseg vLs = sortOn negyelemuMasodik [(vNev v, div (vNepSzam v) (vTerMeret v), vNepSzam v, vTerMeret v) | v <- vLs]

-- 3. a népességszám alapján felépít egy bináros keresőfát, alkalmazva a megfelelő bejárási módot kiírja egy állományba a városokra vonatkozó adatokat a népsségszám alapján rendezve, majd a bináris kersőfát használva megállpítja, hogy melyik a legnagyobb, illetve melyik a legkisebb népességszámmal rendelkező város.
data BST = Empty | Node Varos BST BST deriving (Show) -- Empty ures fa eseten, Node Varos left right eseten van egy Node, ami tartalmaz egy varost es van ket leagazasa (left, right)

-- BST insert
insertBST :: Varos -> BST -> BST
insertBST v Empty = Node v Empty Empty -- nincs leagazas
insertBST v (Node x l r) -- van leagazas
  | vNepSzam v < vNepSzam x = Node x (insertBST v l) r -- bal leagazasba valo beszuras
  | otherwise = Node x l (insertBST v r) -- jobb leagazasba valo beszuras

-- Build BST from list
buildBST :: [Varos] -> BST
buildBST = foldr insertBST Empty -- felepiti a listat jobbrol haladva a listaban

-- In-order traversal
inOrder :: BST -> [Varos]
inOrder Empty        = []
inOrder (Node v l r) = inOrder l ++ [v] ++ inOrder r -- kurrens varos - bal reszfa - jobb reszfa -> bal rf - kurrens - jobb rf

minVaros (Node v Empty _) = v -- addig megy balra lefele, amedding nincs tovabb
minVaros (Node _ l _)     = minVaros l -- ha balra meg van reszfa, megy tovabb

maxVaros (Node v _ Empty) = v -- addig megy jobbra lefele, ameddig nincs tovabb
maxVaros (Node _ _ r)     = maxVaros r -- ha jobbra meg van reszfa, megy tovabb

writeToFile :: FilePath -> [Varos] -> IO ()
writeToFile path vs = writeFile path (unlines (map show vs))

mainII = do
  inf <- openFile "08.labor/varosok.txt" ReadMode
  tartalom <- hGetContents inf
  print tartalom
  let varosok = map toVaros (lines tartalom)
  print varosok
  -- 1.
  putStr "a="
  -- let a = 500
  a <- readLn :: IO Float
  putStr "b="
  -- let b = 1000
  b <- readLn :: IO Float
  let nepsurusegAB = nepsurusegABFg a b varosok
  mapM_ (\v -> putStrLn (fst v ++ " " ++ show (snd v))) nepsurusegAB
  putStrLn ("Varosok szama " ++ show a ++ " es " ++ show b ++ " nepsuruseg kozott " ++ show (length nepsurusegAB))
  -- 2.
  let rendNepsur = rendezNepsuruseg varosok
  mapM_ (\(nev, ns, n, t) -> putStrLn (nev ++ " " ++ show ns ++ " = " ++ show n ++ " / " ++ show t)) rendNepsur
  -- 3.
  let bstVarosok = buildBST varosok
  writeToFile "08.labor/rendezett_varosok.txt" (inOrder bstVarosok)
  let varosMinNepesseg = minVaros bstVarosok
  let varosMaxNepesseg = maxVaros bstVarosok

  putStrLn $ "A legkisebb nepesseggel rendelkezo varos " ++ show varosMinNepesseg
  putStrLn $ "A legnagyobb nepesseggel rendelkezo varos " ++ show varosMaxNepesseg

{- III. Egy listában kriptográfiai algoritmusok parméterei vannak eltárolva.
Három fajta kripto algoritmust tárolhat a lista: StreamCipher, BlockCipher, BlockCipherMode.
Egy StreamCipher típusú adat paraméterei a következők lehetnek: algoritmus név, kulcs méretek,
és protokollok amelyekben használják.
Egy BlockCipher típusú adat paraméterei a következők lehetnek: algoritmus név, kulcs méretek,
blokkméret, és protokollok amelyekben használják.
Egy BlockCipherMode típusú adat paraméterei a következő: algoritmus név.
Pontosabban adott a következő adatszerkezet, illetve konstans lista: -}
type Name = String
type KeyLen = [Int]
type BlockLen = Int
type Protocol = String
data Crypto =
  StreamCipher Name KeyLen [Protocol]
  | BlockCipher Name KeyLen BlockLen [Protocol]
  | BlockCipherMode Name
  deriving (Show, Read, Eq)

lsCrypto = [
  BlockCipher "AES" [128, 192, 256] 128 ["TLS", "PGP", "Kerberos"],
  BlockCipherMode "ECB",
  BlockCipherMode "CBC",
  BlockCipher "Twofish" [128, 192, 256] 128 ["PGP", "Kerberos"],
  StreamCipher "ChaCha20" [128, 256] ["TLS", "S/MIME", "SSH"],
  BlockCipher "3DES" [168] 64 ["TLS", "PGP", "Kerberos"],
  BlockCipherMode "CTR",
  BlockCipherMode "GCM",
  StreamCipher "RC4" [40..2048] ["Kerberos"]
 ]

-- Írjunk egy Haskell-programot, amely a listában levő adatok esetében:

-- 1. meghatározza, hogy hány BlockCipherMode típusú adatot tárol a lista,
isBCM (BlockCipherMode _) = True
isBCM _                   = False

bcmDb = length $ filter isBCM lsCrypto
-- 2. kiválogatja a BlockCipherMode típusú adatokat egy külön listába,
getName :: Crypto -> Maybe String
getName (BlockCipherMode n) = Just n
getName _                   = Nothing
bcmLs = filter isBCM lsCrypto
-- 3. kiválogatja azokat a BlockCipher típusú adatokat amelyek a legtöbb protokollban szerepelnek,
isBC (BlockCipher {}) = True
isBC _                = False

getBC (BlockCipher name keyLen blockLen protocols) = (name, keyLen, blockLen, protocols)

getProtocols :: Crypto -> Maybe [Protocol]
getProtocols (BlockCipher _ _ _ protocols) = Just protocols
getProtocols _                             = Nothing

getProtocolCount :: Crypto -> Maybe Int
getProtocolCount (BlockCipher _ _ _ protocols) = Just (length protocols)
getProtocolCount _                             = Nothing

bcLs = filter isBC lsCrypto

maxBC = map getBC $ filter (\c -> getProtocolCount c == Just maxSzam) bcLs
  where
    maxSzam = maximum [c | Just c <- map getProtocolCount bcLs]
-- 4. kiírja a StreamCipher típusú adatokat, név szerint rendezve egy szövegállományba.
isSC (StreamCipher {}) = True
isSC _                 = False

getSC (StreamCipher name keyLen protocols) = (name, keyLen, protocols)

haromelem1 (x,_,_) = x

scLs = sortOn haromelem1 $ map getSC $ filter isSC lsCrypto

scLsToFile = unlines $ map (\(n,k,p) -> n ++ " [" ++ intercalate "," (map show k) ++ "]"++", [" ++ intercalate "," p ++ "]") scLs

mainIII = do
  -- 1.
  putStrLn ("A lista " ++ show bcmDb ++ " darab BlockCipherMode tipusu adatot tarol:")
  -- 2.
  mapM_ (\n -> case getName n of
                  Nothing -> putStrLn ""
                  Just n  -> putStrLn n ) bcmLs
  -- 3.
  mapM_ (\(n,k,b,p) -> putStrLn (n ++ " [" ++ intercalate ", " (map show k) ++ "] " ++ show b ++ " [" ++ intercalate ", " p ++ "]")) maxBC
  -- 4.
  writeFile "08.labor/sc_adatok.txt" scLsToFile