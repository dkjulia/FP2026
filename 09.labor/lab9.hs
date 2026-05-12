module Lab9 where
import           Data.Char
import           Data.List
import qualified Data.Map as Map -- ghci-n belul :set -package containers
-- # 9. labor

-- I. Formázzuk egy adott szövegállomány tartalmát a következőképpen: azok után az írásjelek után, amelyek benne vannak a $\{.,!?;\}$ halmazban szigorúan egy szóközt tegyünk, hagyjunk.
mainI :: IO ()
mainI = do
    tartalom <- readFile "09.labor/szoveg.txt"
    putStrLn tartalom
    let formazott = (unwords . words) $ concatMap (\x -> if elem x ".,!?;" then x : " " else [x]) tartalom -- az (unwords . words) resz felel azert, hogy szigoruan csak 1 szokoz legyen a szavak kozott
    print formazott
    -- let formazott2 = mapM_ (\x -> if elem x ".,!?;" then putStr (x:" ") else putStr [x]) ((unwords . words) tartalom)
    -- formazott2
    writeFile "09.labor/szoveg_formazott.txt" formazott

-- II. Az [iban.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/iban.txt) állomány IBAN kódokat tartalmaz. Írjunk egy-egy Haskell függvényt, amely

-- 1. beolvassa, majd rendezi az állományban levő adatokat ábécé sorrendbe,
-- 2. bináris keresést alkalmazva ellenőrzi, hogy egy megadott IBAN kód szerepel-e az adatok között,
-- 3. átírja egy okIban.txt állományba azokat az IBAN kódokat, amelyek megfelelő formátumúak. Egy IBAN kód akkor tekinthető megfelelő formátumúnak
--   a. ha csak számjegyeket és angol ábécébeli nagybetűket tartalmaz,
--   b. ha az IBAN kód hossza megegyezik az országhoz tartozó hosszal, ahol az országhoz tartozó hosszérték az [ibanLength.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/ibanLength.txt) állományból olvasható ki,
--   c. ha az átcsoportosítás és a helyettesítés után kapott egész szám 97-el való osztási maradéka egyenlő eggyel, ahol
--     - átcsoportosítás: az IBAN kód első négy karakterét kitöröljük a kód elejéről és a kód végéhez fűzzük,
--     - helyettesítés:
--       - az alfanumerikus karaktereket helyettesítsük a következő kódokkal: $$A \to 10,\ B \to 11,\ \ldots,\ Z \to 35$$
--       - az így kapott karakterláncot egész számnak tekintjük

--   Például:
--   legyen az IBAN kód: $$\texttt{GB82WEST12345698765432}$$
--   - hossz: $$22$$
--   - átcsoportosítás:
--     $$\texttt{WEST12345698765432}\ \texttt{GB82}$$
--   - helyettesítés:
--     $$32142829\quad 12345698765432\quad 1611\quad 82$$
--   - ellenőrzés: $$3214282912345698765432161182 \bmod 97 = 1$$

binarySearch :: (Ord a) => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch x xs =
  let mid = length xs `div` 2
      pivot = xs !! mid
   in case compare x pivot of
        EQ -> True
        LT -> binarySearch x (take mid xs)
        GT -> binarySearch x (drop (mid + 1) xs)

isIbanPresent :: [Char] -> [[Char]] -> [Char]
isIbanPresent iban ibans
  | binarySearch iban ibans = "\n" ++ iban ++ " szerepel az adatok kozott"
  | otherwise = "\n" ++ iban ++ " nem szerepel az adatok kozott"

justNrLttr :: Foldable t => t Char -> Bool
justNrLttr = all isAlphaNum

getIbanLengthByCountry :: Eq t => t -> [(t, a)] -> Maybe a
getIbanLengthByCountry _ [] = Nothing
getIbanLengthByCountry countryToFind ((country, ciLength) : xs)
    | countryToFind == country = Just ciLength
    | otherwise = getIbanLengthByCountry countryToFind xs

ibanCountryCorrect :: [Char] -> [([Char], Int)] -> Bool
ibanCountryCorrect iban countryLengthLs =
  case getIbanLengthByCountry ibanCountry countryLengthLs of
    Just len -> length iban == len
    Nothing -> False
  where
    ibanCountry = take 2 iban

replaceChar :: Char -> [Char]
replaceChar c
  | isDigit c = [c]
  | isUpper c = show (ord c - ord 'A' + 10)
  | otherwise = ""

mod97_1 iban = mod (read helyettesit :: Integer) 97 == 1
    where
        elso4 = take 4 iban
        atcsoportosit = drop 4 iban ++ elso4
        helyettesit = concatMap replaceChar atcsoportosit

isIbanCorrect :: [Char] -> [([Char], Int)] -> Bool
isIbanCorrect iban orszagHosszLs
    | justNrLttr iban && ibanCountryCorrect iban orszagHosszLs && mod97_1 iban = True
    | otherwise = False

-- alternatives 
parseIbanLengths :: String -> Map.Map String Int
parseIbanLengths ibanLengths = Map.fromList $ map ((\[chr, num] -> (chr, read num)) . words) $ lines ibanLengths

hasCorrectLength :: String -> Map.Map String Int -> Bool
hasCorrectLength iban lengthsMap =
  case Map.lookup (take 2 iban) lengthsMap of
    Just len -> length iban == len
    Nothing -> False

mainII :: IO ()
mainII = do
    tartalom <- readFile "09.labor/iban.txt"
    orszagHosszTartalom <- readFile "09.labor/ibanLength.txt"
    let sorok = lines tartalom
        rendezett = sort sorok
        orszagHossz = map (\x -> (take 2 x, read (drop 3 x) :: Int )) $ lines orszagHosszTartalom
    -- 1.
    print rendezett
    -- 2.
    let iban = "TR330006100519786457841326"
        atcsoportositottIban = drop 4 iban ++ take 4 iban
        helyettesitettIban = concatMap replaceChar atcsoportositottIban
        ibanHelytelen = "GB82WEST12345698765132ABC"
    putStrLn (isIbanPresent iban rendezett)
    putStrLn (isIbanPresent ibanHelytelen rendezett)
    --3.
    putStrLn ("IBAN kod: " ++ iban)
    putStrLn ("\t - hossz: " ++ show (length iban))
    putStrLn ("\t - atcsoportositas: " ++ atcsoportositottIban)
    putStrLn ("\t - helyettesites: " ++ helyettesitettIban)
    putStrLn ("\t - ellenorzes: " <> show (mod97_1 iban))
    if isIbanCorrect iban orszagHossz
        then putStrLn (iban ++ " helyes")
        else putStrLn (iban ++ " helytelen")
    let helyesIbanok = filter (\iban -> isIbanCorrect iban orszagHossz) rendezett
    writeFile "09.labor/okIban.txt" (unlines helyesIbanok)
      

{- III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva:
vezetéknév, keresztnév, születési dátum.
Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból
és állapítsuk meg mindegyik személyről, hogy a hét milyen napján született és mikor van a névnapja.
A névnapok megállapításához használhatjuk a [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt) tartalmazó szövegállományt.
A lentebb felhasznalt nevnapok.txt elter ettol, az adatok a kov. formaban talalhatoak benne:
Aba: november 12.
Abel: januar 2., junius 2., augusztus 5., december 5., december 9.-}
data Datum = Datum {
  nap   :: Int,
  honap:: Int,
  ev    :: Int
} deriving (Show)

data Szemely = Szemely {
  vnev    :: [Char],
  knev    :: [Char],
  szdatum :: Datum
} deriving (Show)

data Nevnap = Nevnap {
    nevnapNev:: String,
    datumok:: [String]
}

type NevnapM = Map.Map String [String]

-- szokoev-e az adott ev 
isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- hany nap van az adott honapban az adott evben 
daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month == 2 = if isLeapYear year then 29 else 28
  | month `elem` [4, 6, 9, 11] = 30
  | otherwise = 31

-- helyes datum-e 
isValidDate :: Datum -> Bool
isValidDate (Datum day month year)
  | year < 0 = False
  | month < 1 || month > 12 = False
  | day < 1 || day > daysInMonth month day = False
  | otherwise = True


parseSzemely :: [Char] -> Szemely
parseSzemely line =
  let [vNev, kNev, ev, honap, nap] = words line
      datum = Datum (read nap) (read honap) (read ev)
   in if isValidDate datum then Szemely vNev kNev datum else error "Hibas datum"

dayOfWeek :: Datum -> String
dayOfWeek (Datum d m y) =
  let (m', y') = if m < 3 then (m + 12, y - 1) else (m, y)
      k = y' `mod` 100
      j = y' `div` 100
      h = (d + (13 * (m' + 1)) `div` 5 + k + k `div` 4 + j `div` 4 + 5 * j) `mod` 7
   in ["Szombat", "Vasarnap", "Hetfo", "Kedd", "Szerda", "Csutortok", "Pentek"] !! h

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s =
  let (w, s') = break (== c) s
   in w : case s' of
        [] -> []
        (_ : rest) -> splitBy c rest

parseNevnapok :: String -> Nevnap
parseNevnapok line =
  let (name, rest) = span (/= ':') line
      datumok = map (dropWhile (== ' ')) . splitBy ',' . drop 1 $ dropWhile (/= ':') line
   in Nevnap (map toLower name) datumok

parseNevnapokM :: String -> (String, [String])
parseNevnapokM line =
  let (nev, rest) = span (/= ':') line
      datums = map (dropWhile (== ' ')) . splitBy ',' . drop 1 $ dropWhile (/= ':') line
   in (map toLower nev, datums)

getNameDays :: [Nevnap] -> [Char] -> [String]
getNameDays [] knevSg = error ("Nincs " ++ knevSg ++ " nevnap")
getNameDays (n:maradek) knevSg
    | nevnapNev n == map toLower knevSg = datumok n
    | otherwise = getNameDays maradek knevSg

getNameDaysM :: NevnapM -> String -> [String]
getNameDaysM nevmap name = Map.findWithDefault [] (map toLower name) nevmap

printPerson :: [Nevnap] -> Szemely -> IO ()
printPerson nevnapok (Szemely vNev kNev datum) = do
  putStrLn $ vNev ++ " " ++ kNev ++ ":"
  putStrLn $ "  Születési dátum: " ++ show datum
  putStrLn $ "  Hét napja: " ++ dayOfWeek datum
  let nnap = getNameDays nevnapok kNev
  if null nnap
    then putStrLn "  Névnap: nincs adat"
    else putStrLn $ "  Névnap(ok): " ++ unwords nnap

printPersonM :: NevnapM -> Szemely -> IO ()
printPersonM nevMap (Szemely vNev kNev datum) = do
  putStrLn $ vNev ++ " " ++ kNev ++ ":"
  putStrLn $ "  Születési dátum: " ++ show datum
  putStrLn $ "  Hét napja: " ++ dayOfWeek datum
  let nnap = getNameDaysM nevMap kNev
  if null nnap
    then putStrLn "  Névnap: nincs adat"
    else putStrLn $ "  Névnap(ok): " ++ unwords nnap

mainIII = do
  szemelyekFajl <- readFile "09.labor/szemelyek.txt"
  nevnapokFajl <- readFile "09.labor/nevnapok.txt"

  let szemelyek = map parseSzemely (lines szemelyekFajl)
      nevnapok = (map parseNevnapok (lines nevnapokFajl))
      szemely = parseSzemely "Kovacs Peter 2000 6 29"
      nevMap = Map.fromList (map parseNevnapokM (lines nevnapokFajl))

--   mapM_ (printPerson nevnapok) szemelyek
--   printPerson nevnapok szemely
  mapM_ (printPerson nevnapok) szemelyek
  putStrLn "\nMappel"
  mapM_ (printPersonM nevMap) szemelyek