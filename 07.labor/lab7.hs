module Lab7 where 
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.List
import           Data.Word
import           Numeric
import           System.Directory
import           System.IO
import           System.Random

-- # 7. labor
-- cabal run vagy cabal run lab7, ha cabal-lal van instalalva a System.Random, ehhez kell egy .cabal fajl is

-- Maybe Monad
myHead :: [a] -> Maybe a
myHead []       = Nothing
myHead (k : ve) = Just k

myTail :: [a] -> Maybe [a]
myTail []       = Nothing
myTail (k : ve) = Just ve

-- Olvassunk be szamokat allomanybol, majd irassuk ki allomanyba a rendezett sorrendet
-- allomanybol valo beolvasas
myReadFile f = do
  feof <- hIsEOF f -- ha a fajl vegen vagyunk
  if feof
    then return []
    else do
      temp <- hGetLine f -- kivesszuk az elso sort, a kurzor a vegere kerul
      let k = read temp :: Int -- ha egy szam van egy sorban; az ertek alapjan valtozik, pl. ket elem eseten "2 db" -> [k1,k2] = words temp majd k = (read k1 :: Int, k2)
      ve <- myReadFile f
      return (k : ve)

-- allomanyba iras
myWriteFile f ls = do
  if null ls
    then return ()
    else do
      hPutStrLn f (show k)
      myWriteFile f ve
  where
    k = head ls
    ve = tail ls

main1 = do
  inf <- openFile "07.labor/szamok_be.txt" ReadMode -- a szamok soronkent vannak
  outf <- openFile "07.labor/szamok_ki.txt" WriteMode -- soronkent irjuk ki oket
  ls <- myReadFile inf
  print ls
  -- rendezzuk a szamokat
  let rLs = sort ls
  myWriteFile outf rLs
  hClose inf
  hClose outf

main2 = do
  inf <- openFile "07.labor/szamok_be2.txt" ReadMode -- a szamok egy sorban vannak
  outf <- openFile "07.labor/szamok_ki2.txt" WriteMode -- egy sorban irjuk ki oket
  contents <- hGetContents inf
  let ls = map read (words contents) :: [Int]
      rLs = sort ls
      sRLs = unwords $ map show rLs
  hPutStr outf sRLs
  hClose inf
  hClose outf

myReadWriteToFile1 = do
  handle <- openFile "07.labor/szamok_be_ki.txt" ReadWriteMode
  hPutStrLn handle "23" -- bufferbe kerul
  hFlush handle -- buffer manualis tisztitasa, tartalom a fajlba mentodik
  hSeek handle AbsoluteSeek 0 -- kurzor visszahelyezese a fajl elejere, mert az iras miatt a vegere kerult
  contents <- hGetContents handle
  putStrLn contents
  hClose handle -- kulon be kell zarni a fajlt

myReadWriteToFile2 = withFile "07.labor/szamok_be_ki.txt" ReadWriteMode $ \handle -> do
  hPutStrLn handle "23" -- bufferbe kerul
  hFlush handle -- buffer manualis tisztitasa, tartalom a fajlba mentodik
  hSeek handle AbsoluteSeek 0 -- kurzor visszahelyezese a fajl elejere, mert az iras miatt a vegere kerult
  contents <- hGetContents handle
  putStrLn contents

-- a withFile automatikusan bezarja a fajlt

myAppendToFile = do
  handle <- openFile "07.labor/szoveg.txt" AppendMode
  hPutStrLn handle "hello"
  hClose handle

-- I. Írjunk egy-egy Haskell függvényt, amely

-- - az n-nél kisebb négyzetszámokat kiírja egy szövegállományba,
negyzetSzamok n = takeWhile (< n) [i ^ 2 | i <- [1 ..]]

listaFajlba file ls = do
  if null ls
    then return ()
    else do
      hPutStr file $ show k ++ " "
      listaFajlba file ve
  where
    (k : ve) = ls

negyzetFajlba = do
  putStrLn "n="
  n <- readLn :: IO Int
  let ls = negyzetSzamok n
  writeFile "negyzetSzamok.txt" $ unlines (map show ls)

negyzetFajlba2 = do
  putStrLn "n="
  n <- readLn :: IO Int
  let ls = negyzetSzamok n
  file <- openFile "negyzetSzamok2.txt" WriteMode
  listaFajlba file ls
  hClose file

-- - az n-nél kisebb számok négyzetgyökét kiírja egy szövegállományba. A nr négyzetgyök meghatározásához használjuk a következőket, ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:

--   ```
--   x_0 = 1
--   x_{n+1} = (x_n + nr/x_n)/2
--   ```
negyzetgyok nr = iteral 1
  where
    kovetkezo x = (x + nr / x) / 2
    iteral x
      | kovetkezoX == x = x
      | otherwise = iteral kovetkezoX
      where
        kovetkezoX = kovetkezo x

-- negyzetgyokFajlba "07.labor/negyzetgyokok.txt" 10
negyzetgyokFajlba fajl n = do
  let ls = takeWhile (\(i, ni) -> i < n) $ [(i, negyzetgyok i) | i <- [1 ..]]
  writeFile fajl (unlines (map format ls))
  where
    format (i, ni) = show i ++ " negyzetgyoke " ++ show ni

-- - az n-nél kisebb számok köbgyökét kiírja egy szövegállományba.A köbgyök meghatározásához használjuk a következőket, ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:

--   ```
--   x_0 = 1
--   x_{n+1} = (2·x_n + nr/(x_n·x_n))/3
--   ```
kobgyok nr = iteral 1
  where
    kovetkezo x = (2 * x + nr / (x * x)) / 3
    iteral x
      | abs (kovetkezoX - x) < 1e-10 = x
      | otherwise = iteral kovetkezoX
      where
        kovetkezoX = kovetkezo x

-- kobgyokFajlba "07.labor/kobgyokok.txt" 10
kobgyokFajlba fajl n = do
  let ls = takeWhile (\(i, ni) -> i < n) $ [(i, kobgyok i) | i <- [1 ..]]
  writeFile fajl (unlines (map format ls))
  where
    format (i, ni) = show i ++ " kobgyoke " ++ show ni

-- II. Írjunk egy-egy Haskell függvényt, amely szövegállományban levő számokat olvas be egy listába, és kiírja formázva egy másik szövegállományba

-- - a számok rendezett sorrendjét,
szamokRendezFajlba = do
  tartalom <- readFile "07.labor/szamok_be.txt"
  let szamok = map read (lines tartalom) :: [Int]
      rSzamok = sort szamok
      sRSzamok = unlines $ map show rSzamok
  writeFile "07.labor/rendezettSzamok.txt" sRSzamok

-- - a számokkal együtt a számok 2-es számrendszerbeli alakját, illetve, hogy hány egyes szerepel a 2-es számrendszerbeli alakban,
decToBin 0 = "0"
decToBin n = if n < 0 then reverse (go (abs n)) else reverse (go n)
  where
    go 0 = ""
    go x = show (x `mod` 2) ++ go (x `div` 2)

egyesekSzama n = length $ filter (== '1') n

binFajlba = do
  tartalom <- readFile "07.labor/szamok_be.txt"
  let szamok = map read (lines tartalom) :: [Int]
      ls = [(n, decToBin n, egyesekSzama (decToBin n)) | n <- szamok]
      lsF = unlines $ map (\(n, bin, eSz) -> show n ++ " " ++ show bin ++ " " ++ show eSz) ls
  writeFile "07.labor/szamokKettesSzamrendszerEgyesekSzama.txt" lsF

-- - a számokkal együtt a számok 2, 16, 256-os számrendszerbeli alakját,
decTo16 0 = "0"
decTo16 n = if n < 0 then reverse (go (abs n)) else reverse (go n)
  where
    go 0 = ""
    go x = show (x `mod` 16) ++ go (x `div` 16)

decTo256 0 = "0"
decTo256 n = if n < 0 then reverse (go (abs n)) else reverse (go n)
  where
    go 0 = ""
    go x = show (x `mod` 256) ++ go (x `div` 256)

bin_16_256Fajlba = do
  tartalom <- readFile "07.labor/szamok_be.txt"
  let szamok = map read (lines tartalom) :: [Int]
      ls = [(n, decToBin n, decTo16 n, decTo256 n) | n <- szamok]
      lsF = unlines $ map (\(n, n1, n2, n3) -> show n ++ " " ++ show n1 ++ " " ++ show n2 ++ " " ++ show n3) ls
  writeFile "07.labor/szamok2_16_256.txt" lsF

-- - a számokkal együtt a számok prímosztóit.
primosztok n
  | n < 0 = Nothing
  | n == 0 = Nothing
  | otherwise = Just (oszto n 2)
  where
    oszto 1 _ = []
    oszto m d
      | m `mod` d == 0 = d : oszto (m `div` d) d
      | otherwise = oszto m (d + 1)

primosztokFajlba = do
  tartalom <- readFile "07.labor/szamok_be.txt"
  let szamok = map read (lines tartalom) :: [Int]
      ls = [(n, primosztok n) | n <- szamok]
      lsF = unlines $ map (\(n, mls) -> show n ++ " primosztoi: " ++ unwords (map show (maybe [] id mls))) ls
  writeFile "07.labor/szamokPrimosztok.txt" lsF

-- III. Írjunk egy Haskell függvényt, amely amely kigenerálja egy állományba

-- - az $a$ és $b$ közötti Hamming számokat, használjuk a takeWhile, dropWhile függvényeket ($a > 300$),
-- Hamming szam = csak a 2,3,5 a primosztoja
hamming :: [Int]
hamming =
  1
    : merge3
      (map (2 *) hamming)
      (map (3 *) hamming)
      (map (5 *) hamming)

merge :: [Int] -> [Int] -> [Int]
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs ys

merge3 a b c = merge a (merge b c)

isHamming n = maybe False (all (`elem` [2, 3, 5])) (primosztok n)

hammingAB a b =
  if a < b
    then dropWhile (< a) $ takeWhile (< b) hamming
    else dropWhile (< b) $ takeWhile (< a) hamming

hammingAB2 a b = (dropWhile (< a) . takeWhile (< b)) [i | i <- [2 ..], isHamming i]

hammingAB3 a b = [i | i <- [a .. b], isHamming i]

mainHamming = do
  putStr "a="
  a <- readLn :: IO Int
  putStr "b="
  b <- readLn :: IO Int
  let lsH = hammingAB a b
      lsH2 = if a < b then hammingAB a b else hammingAB b a
  --   writeFile "07.labor/hammingSzamokAB.txt" $ unlines (map show lsH) --kulon sorba
  writeFile "07.labor/hammingSzamokAB.txt" $ unwords (map show lsH) -- egy sorba

-- - az 10000-nél kisebb prímszámokat, a prímszámokat Eratoszthenész szitájával határozzuk meg,
primek = szita [2 ..]
  where
    szita (p : xs) = p : szita [x | x <- xs, x `mod` p /= 0]

primekN n = takeWhile (< n) primek

mainPrimek = do
  let n = 10000
      ls = primekN n
  writeFile "07.labor/primekN.txt" $ unwords (map show ls)

-- - az 10000-nél kisebb szerencsés számokat ([Lucky number](https://en.wikipedia.org/wiki/Lucky_number)).
mySelect :: (Integral a1) => [a2] -> a1 -> [a2]
mySelect ls n = [snd x | x <- filter (fgFilter n) $ zip [1 ..] ls]
  where
    fgFilter n (i, nr) = mod i n /= 0

luckyNr :: (Integral a) => Int -> [a]
luckyNr n = 1 : (take n $ rek 2 [1, 3 ..])

rek :: (Integral a) => Int -> [a] -> [a]
rek i tls = e : rek (i + 1) ls
  where
    ls = mySelect tls e
    e = tls !! (i - 1)

szerencses = 1 : szerencses' [1, 3 ..] 2
  where
    szerencses' xs n =
      let p = xs !! (n - 1)
          xs' = elhagySzam p xs
       in p : szerencses' xs' (n + 1)
    elhagySzam k ys = [y | (y, i) <- zip ys [1 ..], i `mod` k /= 0]

szerencses10000 = takeWhile (< 10000) szerencses

fajlbaIII = do
  putStr "a="
  a <- readLn :: IO Int
  putStr "b="
  b <- readLn :: IO Int
  let h = hammingAB a b
      n = 10000
      p = primekN n
      l = szerencses10000
      file = "07.labor/romai3.txt"

  writeFile file $
    "Hamming szamok ("
      ++ show a
      ++ ", "
      ++ show b
      ++ "):\n"
      ++ show h
      ++ "\n\n"
      ++ "Primszamok 10000 alatt:\n"
      ++ show p
      ++ "\n\n"
      ++ "Szerencses szamok 10000 alatt:\n"
      ++ show l

-- IV. Írjunk egy-egy Haskell függvényt, amely

-- - meghatározza, hogy két bináris állományban milyen pozíciókon található különböző bájt,
-- binKulonbozoPos "noveny.jpg" "noveny.jpg"
-- binKulonbozoPos "noveny.jpg" "oriraskerek.jpg"
binKulonbozoPos :: FilePath -> FilePath -> IO [Int]
binKulonbozoPos fajl1 fajl2 = do
  b1 <- BS.readFile fajl1
  b2 <- BS.readFile fajl2
  return (kulonbozoek (BS.unpack b1) (BS.unpack b2) 0)
  where
    kulonbozoek [] [] _ = []
    kulonbozoek (x : xs) (y : ys) i
      | x /= y = i : maradek
      | otherwise = maradek
      where
        maradek = kulonbozoek xs ys (i + 1)

    -- kulonbozo hossz
    kulonbozoek [] ys i = [i .. i + length ys - 1]
    kulonbozoek xs [] i = [i .. i + length xs - 1]

-- - megvizsgálja, hogy egy adott bájtszekvencia benne van-e egy bináris állományban,
tartBajtszekvencia fajl bsz = do
  let bszFormaz = BC.pack bsz -- amennyiben stringet adunk meg at kell alakitani bajtszekvenciava, amennyiben mar bsz ez a lepes kihagyhato
  content <- BS.readFile fajl
  return (bszFormaz `BS.isInfixOf` content)

-- - meghatározza egy adott állomány bájtméretét, ahol az állománynevet a billentyűzetről olvassuk be,
fajlmeret :: IO ()
fajlmeret = do
  putStrLn "fajl neve:"
  path <- getLine
  handle <- openFile path ReadMode
  size <- hFileSize handle
  hClose handle
  putStrLn ("fajlmeret: " ++ show size ++ " bajt")

-- - meghatározza bináris állományok méret szerinti rendezett sorrendjét, ahol az állományneveket a billentyűzetről olvassuk be,
rendezettBinFajlok = do
  let fajlok = ["07.labor/noveny.jpg", "07.labor/oriaskerek.jpg"]
  pairs <- forM fajlok $ \f -> do
    handle <- openBinaryFile f ReadMode
    size <- hFileSize handle
    hClose handle
    return (f, size)

  let sorted = sortOn snd pairs

  putStrLn "\nFajlok meret szerint rendezve:"
  mapM_ printPair sorted

readFiles :: IO [FilePath]
readFiles = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- readFiles
      return (line : rest)

printPair :: (FilePath, Integer) -> IO ()
printPair (f, s) = putStrLn (f ++ " -> " ++ show s ++ " bytes")

rendezettBinFajlok2 = do
  putStrLn "Add meg a fajlneveket (ures sor = vege):"
  files <- readFiles

  pairs <- forM files $ \f -> do
    -- forM_ is just like for_, but specialised to monadic actions. for_ is just like forM_, but generalised to Applicative actions.
    size <- getFileSize f
    return (f, size)

  let sorted = sortOn snd pairs

  putStrLn "\nFajlok meret szerint rendezve:"
  mapM_ printPair sorted

-- - másolatot készít bináris állományokról, ahol az állományok nevét a billentyűzetről olvassuk be,
masolFajlok = do
  putStrLn "Add meg a fajlneveket (ures sor = vege):"
  --   files <- readFiles
  let files = ["noveny.jpg"]
  eredmenyek <- forM files $ \src -> do
    content <- BS.readFile src
    let dst = takeWhile (/= '.') src ++ "_copy" ++ dropWhile (/= '.') src
    -- let dst = "07.labor/noveny_cp.jpg"
    BS.writeFile dst content
    return True
  if and eredmenyek
    then putStrLn "Masolas kesz!"
    else putStrLn "Nem sikerult!"

-- V. Írjunk egy Haskell programot, amely titkosítja karakterek (bájtok) egy adott listáját, majd vissza is fejti a rejtjelezett értéket:

-- - a titkosításhoz egy titkos információt, egy kulcsot (karaktereket/bájtokat) kell megadni,
-- - a titkosítás azt fogja jelenti, hogy a bemeneti bájtok és a kulcs bájtjai között alkalmazzuk az xor műveletet, úgy hogy a kulcs bájtjait körkörösen vesszük, ami azt jelenti, hogy ha elfogytak a kulcs bájtjai, akkor a kulcs első bájtjával folytatjuk az xor műveletet, egészen addig, amíg a bemenet bájtjain is végig nem mentünk,
-- - a helyes működés miatt fontos, hogy ugyanazt a kulcsot használjuk mind a titkosításhoz, mind a visszafejtéshez,
-- - a titkosított értéket hexadecimális string-ként írjuk ki,
-- - a program során legyen választási lehetőség arra vonatkozóan, hogy a kulcs értékét:
--   - beolvassuk a billentyűzetről, mint hexadecimális string
--   - véletlenszerűen generáljuk, mint 0 és 255 közötti természetes számok.

-- Például:

-- ```haskell
-- > bemenet = "sapientia marosvasarhelyi tudomanyegyetem"
-- > kulcs = "c 38 ff 66 71 22 38 4e 79 65"
-- > cryptStr bemenet kulcs
-- titkositott ertek: 7f 59 8f f 14 4c 4c 27 18 45 61 59 8d 9 2 54 59 3d 18 17 64 5d 93 1f 18 2 4c 3b 1d a 61 59 91 1f 14 45 41 2b d 0 61
-- ```

-- string <-> bajt konverzio
strToBytes :: String -> [Word8]
strToBytes str = map (fromIntegral . fromEnum) str

byteToStr :: [Word8] -> String
byteToStr byte = map (toEnum . fromIntegral) byte

-- hexa atalakitas
toHex :: [Word8] -> String
toHex byte =
  unwords $
    map
      ( \b ->
          let h = showHex b ""
           in if length h == 1
                then '0' : h
                else h
      )
      byte

fromHex :: String -> [Word8]
fromHex str = map (fst . head . readHex) (words str)

-- xor ismetlodo kulccsal
xorWithKey :: [Word8] -> [Word8] -> [Word8]
xorWithKey input key = zipWith xor input (cycle key)

-- titkositas
cryptStr :: String -> [Word8] -> String
cryptStr input key = toHex $ xorWithKey (strToBytes input) key

-- visszafejtes
decryptStr :: String -> [Word8] -> String
decryptStr hexInput key = byteToStr $ xorWithKey (fromHex hexInput) key

-- random generalas
randomKey :: Int -> IO [Word8]
randomKey n = replicateM n (randomRIO (0, 255))

main :: IO ()
main = do
  putStrLn "Bemeneti szoveg:"
  input <- getLine

  putStrLn "Kulcs mod:"
  putStrLn "1 - Hex string"
  putStrLn "2 - Veletlen kulcs"
  mode <- getLine

  key <- case mode of
    "1" -> do
      putStrLn "Add meg a kulcsot (hex, pl: c 38 ff 66):"
      k <- getLine
      return (fromHex k)
    "2" -> do
      putStrLn "Kulcs hossza:"
      len <- readLn
      randomKey len
    _ -> error "Ervenytelen valasztas"

  let encrypted = cryptStr input key

  putStrLn "\nTitkositott (hex):"
  putStrLn encrypted

  let decrypted = decryptStr encrypted key

  putStrLn "\nVisszafejtett:"
  putStrLn decrypted
