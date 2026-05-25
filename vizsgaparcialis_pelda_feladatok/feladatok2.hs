{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Feladatok2 where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ord
import           GHC.Generics
import           Text.Read
{- A diakok.csv a következő adatokat tartalmazza különböző diákokról - név, szak,
évfolyam, átlag és teljesített kreditek száma. Felhasználva a megadott Diak
adatszerkezetet hozzunk létre egy Diakok adatszerkezetet a beolvasott adatok alapján,
figyelve arra, hogy az első sor a fejléc, majd oldjuk meg a következő feladatokat: -}
data Diak = Diak {
    nev          :: String
    , szak       :: String
    , evfolyam   :: Int
    , atlag      :: Double
    , kreditszam :: Int
    } deriving (Show)

newtype Diakok = Diakok {
    diakok :: [Diak]
} deriving Show

makeDiak :: [String] -> Diak
makeDiak [n, sz, evf, atl, hp] =
    Diak
        n
        sz
        (read evf)
        (read atl)
        (read hp)
makeDiak _ = error "Hibas csv sor!"

diakToCSV :: Diak -> [String]
diakToCSV d =
    [ nev d
    , szak d
    , show (evfolyam d)
    , show (atlag d)
    , show (kreditszam d)
    ]

parseCSVLine :: [Char] -> [[Char]]
parseCSVLine = splitOn ","

makeCSVLine :: [[Char]] -> [Char]
makeCSVLine = intercalate ","

readCSV :: FilePath -> IO [[String]]
readCSV filePath = do
    content <- readFile filePath
    let cLs = filter (not . null) (lines content)
    --mapM_ print (take 3 cLs)
    return (map parseCSVLine cLs)

writeCSV :: FilePath -> [[String]] -> IO ()
writeCSV filePath csvData = do
    let content = unlines (map makeCSVLine csvData)
    writeFile filePath content


sortByLexi :: (Ord b, Monad m) => [[b]] -> Int -> m (Maybe [[b]])
sortByLexi ls oszlopIndex = do
    if null $ sortOn (!! oszlopIndex) ls
        then return Nothing
        else return $ Just $ sortOn (!! oszlopIndex) ls


sortByNumeric :: Monad m => [[String]] -> Int -> m (Maybe [[String]])
sortByNumeric ls oszlopIndex = do
    let sLs = map (\sor -> (sor, readMaybe (sor !! oszlopIndex) :: Maybe Double)) ls
        valid = [(sor, num) | (sor, Just num) <- sLs]
        invalid = [sor | (sor, Nothing) <- sLs]
        sortedValid = map fst (sortOn (Down . snd) valid)
    if null sortedValid
        then return Nothing
        else return $ Just $ sortedValid ++ invalid


sortByHeader :: Monad m => [[String]] -> [String] -> String -> m (Maybe [[String]])
sortByHeader ls headerLs headerValue = do
    let oszlopIndex = fromJust (elemIndex headerValue headerLs)
        sLs = map (\sor -> (sor, readMaybe (sor !! oszlopIndex) :: Maybe Double)) ls
        valid = [(sor, num) | (sor, Just num) <- sLs]
        invalid = [sor | (sor, Nothing) <- sLs]
        sortedValid = map fst (sortOn (Down . snd) valid)
    if null sortedValid
    then return Nothing
    else
        return $ Just $ sortedValid ++ invalid

sortCSV :: [[String]] -> Bool -> Bool -> IO (Maybe [[String]])
sortCSV [] _ _ = return Nothing
sortCSV content@(k : ve) numericSort headerSort
    | numericSort && headerSort = return Nothing
    | headerSort = do
        putStrLn $ "fejlec: " ++ intercalate ", " k
        putStrLn "fejlec, kiiratashoz: "
        headerValue <- getLine
        res <- sortByHeader ve k headerValue
        case res of
            Just sorted -> return $ Just $ k : sorted
            Nothing     -> return Nothing
    | numericSort = do
        putStrLn "oszlopindex: "
        temp <- getLine
        let oszlopIndex = read temp :: Int
        res <- sortByNumeric ve oszlopIndex
        case res of
            Just sorted -> return $ Just $ k : sorted
            Nothing     -> return Nothing
    | otherwise = do
        putStrLn "oszlopindex, lexikografikus sorrendhez: "
        temp <- getLine
        let oszlopIndex = read temp :: Int
        res <- sortByLexi ve oszlopIndex
        case res of
            Just sorted -> return $ Just $ k : sorted
            Nothing     -> return Nothing

-- a. Billentyűzetről olvassunk be egy szakot, majd írjuk ki
{- i. az ahhoz tartozó diákok adatait a következő formában:
Például, ha a beolvasott érték Informatika:
Az Informatika szakhoz tartozo diakok:
Anna - 2 - 9.45 - 32
Csilla - 3 - 9.80 - 35
Emese - 1 - 8.74 - 30
Helga - 2 - 9.25 - 31
Karoly - 4 - 9.95 - 38-}
{- ii. a hozzá tartozó átlagot a következő formában:
Az Informatika szak diakjainak atlaga 9.6399 -}
{- iii. a szak legjobb átlagával rendelkező diákot a következő formában:
Az Informatika szak legjobb atlaggal rendelkezo diakja Karoly, atlaga 9.95. -}
{- b. Határozzuk meg, és írassuk ki évfolyamonként a diákok számát és az évfolyam átlagát a következő formában:
1. evfolyam: 4 diak, atlag: 8.2
2. evfolyam: 4 diak, atlag: 8.6
... -}
-- c. Egy kituno.txt állományba írassuk ki mindazon diákok adatait, akik 9.0-nál nagyobb átlaggal és legalább 30 kredittel rendelkeznek.
-- d. Egy rendezett.csv állományba írassuk ki a diákok adatait átlag szerint csökkenő sorrendben, azonos átlag esetén név szerint is rendezzünk.
-- e. Határozzuk meg a legrosszabb és legjobb átlaggal rendelkező diákot.
-- f. Határozzuk meg a legkevesebb és legtöbb kredittel rendelkező diákot.
-- g. Billentyűzetről olvassunk be egy átlagot, majd egy osztondij.csv fájlba írassuk ki mindazon diákokat, akiknek az átlaga az adott értéknél nagyobb vagy azzal egyenlő.
-- h. Határozzuk meg melyik szakon van a legtöbb diák.
-- i. Határozzuk meg melyik szakon a legmagasabb az átlag.
-- j. Billentyűzetről olvassunk be egy új diák adatait, majd adjuk hozzá a diakok.csv fájlhoz, megtartva a korábbi diákokat.

printDiak :: Diak -> IO ()
printDiak d = putStrLn (nev d ++ " - " ++ show (evfolyam d) ++ " - " ++ show (atlag d) ++ " - " ++ show (kreditszam d))


mainI :: IO ()
mainI = do
    let path = "vizsgaparcialis_pelda_feladatok/"
        inputFile = path++"diakok.csv"
    putStrLn $ "Beolvasás: " ++ inputFile
    tartalom <- readCSV inputFile
    if null tartalom
        then putStrLn "Hiba: Ures csv fajl!"
        else do
            let ds = map makeDiak (tail tartalom)
            -- a.
            putStrLn "Adj meg egy szakot (Informatika, Matematika, Fizika)"
            szakBe <- getLine
            let szakDiakok = filter (\d -> szak d == szakBe) ds
            if null szakDiakok then putStrLn "nincs ilyen szak"
            else do
                let szakAtlagok = map (atlag) szakDiakok
                    szakAtlag = sum szakAtlagok / fromIntegral (length szakAtlagok)
                    szakLegjobb = maximumBy (comparing atlag) szakDiakok
                -- a.i.
                putStrLn (szakBe ++ " diakjai: ")
                mapM_ printDiak szakDiakok
                -- a.ii.
                putStrLn (szakBe ++ " diakjainak atlaga " ++ show szakAtlag)
                -- a.iii.
                putStrLn (szakBe ++ " legjobb atlaggal rendelkezo diakja: " ++ nev szakLegjobb ++ ", atlaga " ++ show (atlag szakLegjobb))
            -- b.
            let evfolyamok = (sort . nub . map evfolyam) ds
            mapM_ (\evf -> let evfLs = filter (\d -> evfolyam d == evf) ds
                               db = length evfLs
                               atl = sum (map atlag evfLs) / fromIntegral (length evfLs)
                            in putStrLn $
                            show evf
                            ++ " evfolyam: " ++ show db
                            ++ " diak, atlag: " ++ show atl
                ) evfolyamok
            -- c.
            let kitunoLs = filter (\d -> atlag d > 9.0 && kreditszam d >= 30) ds
                kitunoStr = if null kitunoLs then "nincsenek kituno diakok" else unlines [nev d ++ " - " ++ show (atlag d) ++ " - " ++ show (kreditszam d) | d <- kitunoLs]
            writeFile (path++"kituno.txt") kitunoStr
            -- d.
            let rendezettDs = sortBy (\a b -> compare (atlag b) (atlag a) <> compare (nev a) (nev b)) ds
                csvDs = ["nev","szak","evfolyam","atlag","hitelpont"] : map diakToCSV rendezettDs
            writeCSV (path++"rendezett.csv") csvDs
            -- e.
            let legjobbAtlag = maximumBy (comparing atlag) ds
                legrosszabbAtlag = minimumBy (comparing atlag) ds
            putStrLn ("A legrosszabb atlaggal rendelkezo diak " ++ nev legrosszabbAtlag ++ ", atlaga " ++ show (atlag legrosszabbAtlag))
            putStrLn ("A legjobb atlaggal rendelkezo diak " ++ nev legjobbAtlag ++ ", atlaga " ++ show (atlag legjobbAtlag))
            -- f.
            let legtobbKredit = maximumBy (comparing kreditszam) ds
                legkevesebbKredit = minimumBy (comparing kreditszam) ds
            putStrLn ("A legkevesebb kredittel rendelkezo diak " ++ nev legkevesebbKredit ++ ", kreditei szama " ++ show (kreditszam legkevesebbKredit))
            putStrLn ("A legtobb kredittel rendelkezo diak " ++ nev legtobbKredit ++ ", kreditei szama " ++ show (kreditszam legtobbKredit))
            -- g.
            putStrLn "Adj meg egy atlagot"
            atlagHatar <- readLn :: IO Double
            let nagyobbAtlag = filter (\d -> atlag d >= atlagHatar) ds
                diakokCSV = map diakToCSV nagyobbAtlag
            writeCSV (path++"osztondij.csv") diakokCSV
            -- h.
            let szakonkentiLetszam = map (\ls ->(head ls, length ls)) $ (group . sort . map szak) ds
                maxLetszam = snd $ maximumBy (comparing snd) szakonkentiLetszam
                maxLetszamuSzakok = filter (\e -> snd e == maxLetszam) szakonkentiLetszam
            putStrLn ("A legtobb diak a kovetkezo szakon/szakokon van: " ++ intercalate ", " (map fst maxLetszamuSzakok))
            -- i.
            let szakok = (nub . map szak) ds
                szakonkentiAtlag =  map (\sz ->
                                            let atlagok = [atlag d | d <- ds, szak d == sz]
                                                a = sum atlagok / fromIntegral (length atlagok)
                                            in (sz, a)
                                        ) szakok
                maxSzakAtlag = maximumBy (comparing snd) szakonkentiAtlag
            putStrLn (fst maxSzakAtlag ++ " szakon a legmagasabb az atlag " ++ show (snd maxSzakAtlag))
            -- j.
            putStrLn "Add meg egy uj diak adatait szokozzel elvalasztva (nev, szak, evfolyam, atlag, kreditszam)"
            ujDiak <- getLine
            let ujDiakParsed = makeDiak (words ujDiak)
                dsUj = ujDiakParsed : ds
                ujCSV = map diakToCSV dsUj
            writeCSV (path++"diakok.csv") ujCSV

{- A filmek.json filmek adatait tárolja: cím, rendező, műfaj, év, értékelés, színészek,
akik a filmben játszottak. Az adatok alapján hozzunk létre egy Film és Filmek rekord
típust, majd beolvasva az adatokat végezzük el a következő feladatokat: -}
-- a. Rendezzük a filmeket év szerint rendezve, majd írassuk ki őket könnyen olvasható formában.
-- b. Olvassunk be egy műfajt a billentyűzetről, majd határozzuk meg:
{- i. az adott műfajú filmeket, írassuk ki a következő formában:
A Sci-Fi mufaju filmek:
Inception (2010) rendezte Christopher Nolan
Interstellar (2014) rendezte Christopher Nolan
The Matrix (1999) Wachowski Sisters
Avatar (2009) James Cameron -}
{- ii. a műfaj átlag értékelését, írassuk ki a következő formában:
A Sci-Fi atlag ertekelese 8.775. -}
{- iii. a legjobb értékelésű filmet a műfajban, írassuk ki a következő formában:
A Sci-Fi mufaj legjobban ertekelt muve Interstellar
(2014), amit Christopher Nolan rendezett, az ertekeles amit kapott
9.3. -}
-- c. Határozzuk meg melyik rendezőnek van a legtöbb filmje.
-- d. Határozzuk meg melyik színész szerepel a legtöbb filmben.
-- e. Határozzuk meg melyik rendező dolgozott a legtöbb színésszel.
-- f. Egy top_filmek.txt állományba írassuk ki a 9.0-nál nagyobb vagy azzal egyenlő értékeléssel rendelkező filmeket.
-- g. Egy rendezok.txt fajlba írassuk ki a rendezők adatait a következő formában: rendező neve, filmjeinek száma, átlag értékelése.

data Film = Film {
    cim         :: String
    , rendezo   :: String
    , mufaj     :: String
    , ev        :: Int
    , ertekeles :: Double
    , szineszek :: [String]
} deriving (Show, Read, Generic, FromJSON, ToJSON)

newtype Filmek = Filmek {
    filmek :: [Film]
} deriving (Show, Read, Generic, FromJSON, ToJSON)


-- altalanos json beolvasas
beolvas :: FromJSON a => FilePath -> IO (Maybe a)
beolvas fajl = do
    content <- B.readFile fajl
    return (decode content)

kiirFilm :: Film -> IO ()
kiirFilm f = putStrLn (cim f ++ ", rendezte " ++ rendezo f ++ ", mufaja " ++ mufaj f ++ ", megjelent " ++ show (ev f) ++ ", ertekelese " ++ show (ertekeles f) ++ ", szineszei: " ++ intercalate ", " (szineszek f))

strFilm :: Film -> [Char]
strFilm f = cim f ++ ", rendezte " ++ rendezo f ++ ", mufaja " ++ mufaj f ++ ", megjelent " ++ show (ev f) ++ ", ertekelese " ++ show (ertekeles f) ++ ", szineszei: " ++ intercalate ", " (szineszek f)

strRendezo :: (Show a1, Show a2) => ([Char], a1, a2) -> [Char]
strRendezo (rNev, rFilmSz, rAtlErt) = rNev ++ " " ++ show rFilmSz ++ " filmet rendezett, ertekelese " ++ show rAtlErt

mainII :: IO ()
mainII = do
    let path = "vizsgaparcialis_pelda_feladatok/"
        inputFile = path++"filmek.json"
    tartalom <- beolvas inputFile
    case tartalom of
        Nothing          -> putStrLn "Hibas JSON fajl!"
        Just (Filmek fs) -> do
            -- mapM_ kiirFilm fs
            -- a.
            let rendezettEv = sortOn ev fs
            mapM_ kiirFilm rendezettEv
            -- b.
            putStrLn "Adj meg egy mufajt (Sci-Fi, Action, Crime, Western, Drama, Historical, Horror)"
            mufajBe <- getLine
            --b.i.
            let mufajFilmek = filter (\f -> mufaj f == mufajBe) fs
            if null mufajFilmek
                then putStrLn "Nincs ilyen mufaj"
                else do
                    putStrLn "A Sci-Fi mufaju filmek:"
                    mapM_ (\f -> putStrLn (cim f ++ " (" ++ show (ev f) ++ ")" ++ " rendezte " ++ rendezo f)) mufajFilmek
                    --b.ii.
                    let mufajErtekeles = map ertekeles mufajFilmek
                        mufajAtlagErtekeles = sum mufajErtekeles / fromIntegral (length mufajErtekeles)
                    putStrLn ("A" ++ mufajBe ++ " atlag ertekelese " ++ show mufajAtlagErtekeles ++ ".")
                    --b.iii.
                    let legjobbErtekelesu = maximumBy (comparing ertekeles) mufajFilmek
                    putStrLn ("A " ++ mufajBe ++ " mufaj legjobban ertekelt muve "
                                ++ cim legjobbErtekelesu ++ " (" ++ show (ev legjobbErtekelesu)
                                ++ "), amit " ++ rendezo legjobbErtekelesu
                                ++ " rendezett, az ertekeles amit kapott "
                                ++ show (ertekeles legjobbErtekelesu) ++ ".")
            --c.
            let rendezok = nub . map rendezo $ fs
                rendezoFilmek = map (\r ->
                    let filmekRendezo=filter (\f -> rendezo f == r ) fs
                    in (r, length filmekRendezo)
                    ) rendezok
                maxRendezoFilmek = maximumBy (comparing snd) rendezoFilmek
            putStrLn ("A legtobb filmet rendezte " ++ fst maxRendezoFilmek ++ ", " ++ show (snd maxRendezoFilmek))
            --d.
            let szineszekSzereples = map (\sz -> (head sz, length sz)) $ group . sort . concatMap szineszek $ fs
                maxSzineszSzereples = maximumBy (comparing snd) szineszekSzereples
            putStrLn ("A legtobb filmben jatszo szinesz " ++ fst maxSzineszSzereples ++ ", " ++ show (snd maxSzineszSzereples))
            --e.
            let rendezoSzineszek = map (\r ->
                    let rSz = map szineszek $ filter (\f -> rendezo f == r) fs
                    in (r, length . nub . concat $ rSz)
                    ) rendezok
                maxRendezoSzineszek = [(r, sz) | (r,sz) <- rendezoSzineszek, sz == maxSz]
                    where maxSz = snd $ maximumBy (comparing snd) rendezoSzineszek
            putStrLn ("A legtobb szinesszel dolgozott " ++ intercalate ", " (map fst maxRendezoSzineszek) ++ ", szineszek szama " ++ show (snd . head $ maxRendezoSzineszek))
            --f.
            let topFilmek = filter (\f -> ertekeles f >= 9.0) fs
                topFilmekStr = map strFilm topFilmek
            writeFile (path++"top_filmek.txt") (intercalate "\n" topFilmekStr)
            --g. rendező neve, filmjeinek száma, átlag értékelése
            let rendezoAdat = map (\r ->
                    let filmSzam = length [cim f | f <- fs, rendezo f == r]
                        ertekelesR = [ertekeles f | f <- fs, rendezo f == r]
                        atlagErtekeles = sum ertekelesR / fromIntegral (length ertekelesR)
                    in (r, filmSzam, atlagErtekeles)
                    ) rendezok
                rendezoAdatStr = map strRendezo rendezoAdat
            writeFile (path++"rendezok.txt") (intercalate "\n" rendezoAdatStr)

