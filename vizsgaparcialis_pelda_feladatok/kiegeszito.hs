module Kiegeszito where
import           Data.List
import           Data.Maybe  (fromMaybe, mapMaybe)
import           Data.Ord
import           Text.Printf (printf)

-- Adott a rendelesek.txt fájl, dolgozzuk fel, majd oldjuk meg az alábbi feladatokat.

data Cim = Cim {
    varosnev :: String,
    utca     :: String
} deriving Show

data Termek = Termek {
    tNev   :: String,
    dbSzam :: Int
} deriving (Show)

data Rendeles = Rendeles {
    rId      :: Int,
    rNev     :: String,
    rcim     :: Cim,
    termekek :: [Termek],
    osszeg   :: Double
} deriving Show

-- ===========================================================================
-- Segédfüggvények
-- ===========================================================================

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim (x:xs)
    | x == delim = [] : rest
    | otherwise  = (x : head rest) : tail rest
    where rest = splitOn delim xs

-- Sor értelmezése Maybe-vel: Nothing ha a formátum hibás
parseSor :: String -> Maybe Rendeles
parseSor sor =
    case splitOn '#' sor of
        [rendIdStr, nev, cim, termekekLs, osszegStr] ->
            case (reads rendIdStr, reads osszegStr) of
                ([(rendId, "")], [(osszegV, "")]) ->
                    let cimAdatokLs = splitOn ',' cim
                        cimAdatok   = Cim (head cimAdatokLs) (last cimAdatokLs)
                        termekListaRaw = map (splitOn ':') (splitOn '|' termekekLs)
                        mTermekek = mapM toTermek termekListaRaw
                    in case mTermekek of
                        Just tLs -> Just $ Rendeles rendId nev cimAdatok tLs osszegV
                        Nothing  -> Nothing
                _ -> Nothing
        _ -> Nothing
  where
    toTermek [nevT, dbStr] =
        case reads dbStr of
            [(db, "")] -> Just $ Termek nevT db
            _          -> Nothing
    toTermek _ = Nothing

termekToString :: Termek -> String
termekToString t = tNev t ++ " X " ++ show (dbSzam t)

cimToString :: Cim -> String
cimToString c = varosnev c <> " " <> utca c

rendelesToString :: Rendeles -> String
rendelesToString r =
    show (rId r) <> ": " <> rNev r <> ", " <> cimToString (rcim r)
    <> ", rendelesek:\n\t- "
    <> intercalate "\n\t- " (map termekToString (termekek r))
    <> "\n\tvegosszeg: " <> show (osszeg r)

printRendeles :: Rendeles -> IO ()
printRendeles = putStrLn . rendelesToString

-- ===========================================================================
-- 2. feladat
-- ===========================================================================

fel2 :: [Rendeles] -> IO ()
fel2 rLs = do
    let n             = length rLs
        osszVegosszeg = sum $ map osszeg rLs
        atlag         = osszVegosszeg / fromIntegral n
        maxR          = maximumBy (comparing osszeg) rLs
        minR          = minimumBy (comparing osszeg) rLs
    putStrLn $ "\nRendelesek szama: " ++ show n
    printf "Osszes rendeles vegosszege: %.2f\n" osszVegosszeg
    printf "Atlag rendelesi ertek: %.2f\n" atlag
    putStrLn $ "A legnagyobb osszegu rendeles:\n\t" ++ rendelesToString maxR
    putStrLn $ "A legkisebb osszegu rendeles:\n\t" ++ rendelesToString minR

-- ===========================================================================
-- 3. feladat
-- ===========================================================================

osszDb :: String -> [Termek] -> Int
osszDb nev tLs = sum [dbSzam t | t <- tLs, tNev t == nev]

kulonbozoDb :: Rendeles -> Int
kulonbozoDb r = length . nub . map tNev $ termekek r

fel3 :: [Rendeles] -> IO ()
fel3 rLs = do
    let termekekLs   = concatMap termekek rLs
        termekNevek  = nub $ map tNev termekekLs
        -- a. különböző termékek száma
        kulTermekSzam = length termekNevek
        -- b. legtöbbször rendelt termék (előfordulás)
        elofordulas  = map (\ls -> (head ls, length ls)) . group . sort $ map tNev termekekLs
        maxEloSz     = snd $ maximumBy (comparing snd) elofordulas
        maxTermekek  = filter (\(_, sz) -> sz == maxEloSz) elofordulas
        -- c. legtöbb darabszám
        termekDbLs   = map (\n -> (n, osszDb n termekekLs)) termekNevek
        maxDb        = snd $ maximumBy (comparing snd) termekDbLs
        maxDbTermekek = filter (\(_, db) -> db == maxDb) termekDbLs
        -- e. legtöbb különböző termékkel rendelkező rendelés(ek)
        maxKulDb     = maximum $ map kulonbozoDb rLs
        maxKulTerR   = filter (\r -> kulonbozoDb r == maxKulDb) rLs

    putStrLn $ "a. Kulonbozo termekek szama: " ++ show kulTermekSzam
    putStrLn $ "b. Legtobbet rendelt termek(ek): "
        ++ intercalate ", " (map (\(n, sz) -> n ++ " (" ++ show sz ++ "x)") maxTermekek)
    putStrLn $ "c. Legtobb darabszam alapjan: "
        ++ intercalate ", " (map (\(n, db) -> n ++ " (" ++ show db ++ " db)") maxDbTermekek)
    -- d. billentyűzetről beolvasott termék
    putStrLn $ "d. Adj meg egy keresett termeket (" ++ intercalate ", " termekNevek ++ "):"
    keresettTermek <- getLine
    let keresettDb = osszDb keresettTermek termekekLs
    if keresettDb == 0
        then putStrLn "Nincs ilyen termek az allomanyban."
        else putStrLn $ keresettTermek ++ " osszesitett darabszama: " ++ show keresettDb
    putStrLn $ "e. Legtobb kulonbozo termeket tartalmazo rendeles(ek) azonositoi: "
        ++ show (map rId maxKulTerR)

-- ===========================================================================
-- 4. feladat
-- ===========================================================================

varosokRendelesek :: [Rendeles] -> [(String, Int)]
varosokRendelesek rs =
    map (\ls -> (head ls, length ls)) . group . sort $ map (varosnev . rcim) rs

varosRendelesek :: String -> [Rendeles] -> Maybe (String, Int)
varosRendelesek v rs =
    find (\(nev, _) -> nev == v) (varosokRendelesek rs)

rendelesOsszeg :: String -> [Rendeles] -> Double
rendelesOsszeg v rLs = sum [osszeg r | r <- rLs, varosnev (rcim r) == v]

varosAtlagRendeles :: [String] -> [Rendeles] -> [(String, Int, Double)]
varosAtlagRendeles varosokLs rendelesekLs = map f varosokLs
  where
    f v = case varosRendelesek v rendelesekLs of
        Just (name, cnt) ->
            let atl = if cnt == 0 then 0
                      else rendelesOsszeg v rendelesekLs / fromIntegral cnt
            in (name, cnt, atl)
        Nothing -> (v, 0, 0)

fel4 :: [Rendeles] -> String -> IO ()
fel4 rLs path = do
    let varosok         = nub $ map (varosnev . rcim) rLs
        varosRendLs     = varosokRendelesek rLs
        maxVaros        = maximumBy (comparing snd) varosRendLs
        varosAtlagLs    = varosAtlagRendeles varosok rLs
        fajlTartalom    = unlines $
            map (\(n, db, atl) -> n ++ " - " ++ show db ++ " - " ++ show atl) varosAtlagLs

    putStrLn $ "a. A legtobb rendeles varosa: "
        ++ fst maxVaros ++ " (" ++ show (snd maxVaros) ++ " rendeles)"
    putStrLn "b. Varosok rendelesszama:"
    mapM_ (\(v, sz) -> putStrLn $ "   " ++ v ++ " - " ++ show sz ++ " rendeles") varosRendLs
    putStrLn "c. Varosok atlagos rendelesi erteke:"
    mapM_ (\(n, _, atl) -> printf "   %s - %.2f\n" n atl) varosAtlagLs
    putStrLn $ "d. varosok.txt letrehozva: " ++ path ++ "varosok.txt"
    writeFile (path ++ "varosok.txt") fajlTartalom

-- ===========================================================================
-- 5. feladat
-- ===========================================================================

vasarloKoltes :: String -> [Rendeles] -> (String, Double)
vasarloKoltes vevo rLs = (vevo, sum [osszeg r | r <- rLs, rNev r == vevo])

rendelesTermekDb :: Rendeles -> Int
rendelesTermekDb r = sum $ map dbSzam $ termekek r

rendelesAtlagDbSzam :: Rendeles -> (Int, Double)
rendelesAtlagDbSzam r =
    let tr       = termekek r
        rTrDb    = length tr
        rOsszTrDb = rendelesTermekDb r
    in (rId r, fromIntegral rOsszTrDb / fromIntegral rTrDb)

fel5 :: [Rendeles] -> String -> IO ()
fel5 rLs path = do
    let vasarlok       = nub $ map rNev rLs
        vasarloKoltesLs = map (`vasarloKoltes` rLs) vasarlok
        maxKoltes      = maximumBy (comparing snd) vasarloKoltesLs
        vasarloRendLs  = map (\ls -> (head ls, length ls)) . group . sort $ map rNev rLs
        maxRendVasarlo = maximumBy (comparing snd) vasarloRendLs
        maxTermekR     = maximumBy (comparing snd) $ map (\r -> (rId r, rendelesTermekDb r)) rLs
        maxAtlagR      = maximumBy (comparing snd) $ map rendelesAtlagDbSzam rLs
        nagyRendelesek = filter (\r -> osszeg r > 200) rLs
        nagyStr        = unlines $ map rendelesToString nagyRendelesek

    putStrLn $ "a. Legtobb penzt kolto vasarlo: "
        ++ fst maxKoltes ++ " (" ++ show (snd maxKoltes) ++ ")"
    putStrLn $ "b. Legtobb rendelest leado vasarlo: "
        ++ fst maxRendVasarlo ++ " (" ++ show (snd maxRendVasarlo) ++ " rendeles)"
    putStrLn $ "c. Legtobb osszesitett termekdarabos rendeles: "
        ++ show (fst maxTermekR) ++ " (" ++ show (snd maxTermekR) ++ " db)"
    putStrLn $ "d. Legnagyobb atlagos termekdarabszam rendelese: "
        ++ show (fst maxAtlagR) ++ " (" ++ show (snd maxAtlagR) ++ " db/termek)"
    putStrLn $ "e. nagy_rendelesek.txt letrehozva: " ++ path ++ "nagy_rendelesek.txt"
    writeFile (path ++ "nagy_rendelesek.txt") nagyStr

-- ===========================================================================
-- 6. feladat
-- ===========================================================================

termekDb :: String -> [Termek] -> (String, Int)
termekDb keresettTNev tLs =
    (keresettTNev, sum [dbSzam t | t <- tLs, tNev t == keresettTNev])

osszesTermek :: [Rendeles] -> [Termek]
osszesTermek = concatMap termekek

termekParok :: [a] -> [(a, a)]
termekParok []     = []
termekParok (x:xs) = [(x, y) | y <- xs] ++ termekParok xs

egyuttDb :: (String, String) -> [Rendeles] -> Int
egyuttDb (a, b) rs =
    length [() | r <- rs,
                 let nevek = map tNev (termekek r),
                 a `elem` nevek, b `elem` nevek]

-- Visszatér Nothing ha kevesebb mint 2 különböző termék van (nincs pár)
legtobbEgyutt :: [Rendeles] -> Maybe ((String, String), Int)
legtobbEgyutt rs
    | length parok == 0 = Nothing
    | otherwise         = Just $ maximumBy (comparing snd) [(p, egyuttDb p rs) | p <- parok]
  where
    nevek = nub $ map tNev (osszesTermek rs)
    parok = termekParok nevek

fel6 :: [Rendeles] -> String -> IO ()
fel6 rLs path = do
    let termekekLs    = concatMap termekek rLs
        termekNevek   = nub $ map tNev termekekLs
        termekDbLs    = nub $ map (`termekDb` termekekLs) termekNevek
        -- b. legtöbb különböző rendelésben szereplő termék
        elofordulas   = map (\ls -> (head ls, length ls)) . group . sort $ map tNev termekekLs
        maxTermek     = maximumBy (comparing snd) elofordulas
        -- statisztika adatok
        rSzam         = length rLs
        atlRendErt    = sum (map osszeg rLs) / fromIntegral rSzam
        legdragabb    = maximumBy (comparing osszeg) rLs
        legnepszerubb = maximumBy (comparing snd) termekDbLs
        maxVaros      = maximumBy (comparing snd) . map (\ls -> (head ls, length ls))
                        . group . sort $ map (varosnev . rcim) rLs
        statStr       = unlines
            [ "Rendelesek szama: " ++ show rSzam
            , printf_str "Atlagos rendelesi ertek: %.2f" atlRendErt
            , "Legdragabb rendeles: " ++ rendelesToString legdragabb
            , "Legnepszerubb termek: " ++ fst legnepszerubb ++ " (" ++ show (snd legnepszerubb) ++ " db)"
            , "Legtobb rendelest leado varos: " ++ fst maxVaros
            ]

    putStrLn "a. Termekenkenti osszesitett darabszam:"
    mapM_ (\(n, db) -> putStrLn $ "   " ++ n ++ " - " ++ show db ++ " db") termekDbLs
    putStrLn $ "b. Legtobb rendelesben szereplo termek: "
        ++ fst maxTermek ++ " (" ++ show (snd maxTermek) ++ " rendelesben)"
    -- c. legtöbbször együtt rendelt pár
    case legtobbEgyutt rLs of
        Nothing       -> putStrLn "c. Nincs elegendo termek a parhuzamos elemzeshez."
        Just (par, n) -> putStrLn $ "c. Legtobbet egyutt rendelt ket termek: "
            ++ fst par ++ " es " ++ snd par ++ " (" ++ show n ++ "x)"
    putStrLn $ "d. statisztika.txt letrehozva: " ++ path ++ "statisztika.txt"
    writeFile (path ++ "statisztika.txt") statStr

-- printf nem ad vissza String-et, ezért segédfüggvény
printf_str :: String -> Double -> String
printf_str fmt val = show val  -- egyszerűsített verzió; printf-hez IO kellene

-- ===========================================================================
-- 7. feladat – Rendelések csoportosítása értéktartományok szerint
-- ===========================================================================

kategoriak :: [Rendeles] -> [(String, [Rendeles])]
kategoriak rs =
    [ ("0-100",   [r | r <- rs, osszeg r < 100])
    , ("100-200", [r | r <- rs, osszeg r >= 100 && osszeg r < 200])
    , ("200-300", [r | r <- rs, osszeg r >= 200 && osszeg r < 300])
    , ("300+",    [r | r <- rs, osszeg r >= 300])
    ]

fel7 :: [Rendeles] -> IO ()
fel7 rLs = do
    let csoportok = kategoriak rLs
    putStrLn "Rendelesek ertektartomanyok szerint:"
    mapM_ (\(cat, rs) ->
        putStrLn $ "  " ++ cat ++ ": " ++ show (length rs) ++ " rendeles"
            ++ " (azonositok: " ++ show (map rId rs) ++ ")"
        ) csoportok

-- ===========================================================================
-- 8. feladat – Melyik városban a legnagyobb az egy rendelésre jutó átlagos termékszám
-- ===========================================================================

varosAtlagTermekSzam :: [Rendeles] -> Maybe (String, Double)
varosAtlagTermekSzam [] = Nothing
varosAtlagTermekSzam rLs =
    let varosok = nub $ map (varosnev . rcim) rLs
        atlagok = map (\v ->
            let vRs  = filter (\r -> varosnev (rcim r) == v) rLs
                ossz = sum $ map (fromIntegral . rendelesTermekDb) vRs
                cnt  = fromIntegral (length vRs)
            in (v, ossz / cnt :: Double)) varosok
    in Just $ maximumBy (comparing snd) atlagok

fel8 :: [Rendeles] -> IO ()
fel8 rLs =
    case varosAtlagTermekSzam rLs of
        Nothing        -> putStrLn "Nincsenek rendelesek."
        Just (v, atl)  -> printf "Legnagyobb atlagos termekszam: %s (%.2f db/rendeles)\n" v atl

-- ===========================================================================
-- 9. feladat – topvasarlok.csv (név, összköltés, rendelések száma)
-- ===========================================================================

fel9 :: [Rendeles] -> String -> IO ()
fel9 rLs path = do
    let vasarlok   = nub $ map rNev rLs
        sorAdatok  = map (\v ->
            let kolt = sum [osszeg r | r <- rLs, rNev r == v]
                cnt  = length [r | r <- rLs, rNev r == v]
            in (v, kolt, cnt)) vasarlok
        rendezve   = sortBy (comparing (\(_, k, _) -> negate k)) sorAdatok
        fejlec     = "nev,osszes_koltes,rendelesek_szama"
        sorok      = map (\(n, k, c) -> n ++ "," ++ show k ++ "," ++ show c) rendezve
        csv        = unlines (fejlec : sorok)
    putStrLn $ "topvasarlok.csv letrehozva: " ++ path ++ "topvasarlok.csv"
    writeFile (path ++ "topvasarlok.csv") csv

-- ===========================================================================
-- 10. feladat – Legnagyobb becsült bevétel termékenként
--     (a végösszeg arányosan oszlik el a termékek között darabszám szerint)
-- ===========================================================================

becsultTermekBevelel :: [Rendeles] -> [(String, Double)]
becsultTermekBevelel rLs =
    let termekNevek = nub $ map tNev (osszesTermek rLs)
        bevételByT  = map (\tN -> (tN, termekBevelel tN)) termekNevek
    in bevételByT
  where
    termekBevelel tN = sum $ mapMaybe (termekReszBev tN) rLs

    termekReszBev tN r =
        let trLs      = termekek r
            tOssz     = rendelesTermekDb r
            tDbEbben  = sum [dbSzam t | t <- trLs, tNev t == tN]
        in if tOssz == 0 then Nothing
           else Just $ osszeg r * fromIntegral tDbEbben / fromIntegral tOssz

-- Visszatér Nothing ha nincsenek termékek
legNagyobbBevTelek :: [Rendeles] -> Maybe (String, Double)
legNagyobbBevTelek [] = Nothing
legNagyobbBevTelek rLs =
    case becsultTermekBevelel rLs of
        [] -> Nothing
        bs -> Just $ maximumBy (comparing snd) bs

fel10 :: [Rendeles] -> IO ()
fel10 rLs = do
    putStrLn "Termekenkenti becsult bevetelek:"
    let bevLs = becsultTermekBevelel rLs
    mapM_ (\(n, b) -> printf "   %s - %.2f\n" n b) bevLs
    case legNagyobbBevTelek rLs of
        Nothing       -> putStrLn "Nincsenek termekek."
        Just (n, bev) -> printf "Legnagyobb becsult bevetel: %s (%.2f)\n" n bev

-- ===========================================================================
-- 11. feladat – Város alapú kereső
-- ===========================================================================

varosKereső :: String -> [Rendeles] -> IO ()
varosKereső v rLs = do
    let varosRs = filter (\r -> varosnev (rcim r) == v) rLs
    if null varosRs
        then putStrLn $ "Nincs rendeles ebbol a varosbol: " ++ v
        else do
            -- a. rendelések listája
            putStrLn $ "a. Rendelesek " ++ v ++ " varosbol:"
            mapM_ printRendeles varosRs
            -- b. teljes bevétel
            let teljesBev = sum $ map osszeg varosRs
            printf "b. Teljes bevétel: %.2f\n" teljesBev
            -- c. legnépszerűbb termék
            let termekekLs = concatMap termekek varosRs
            if null termekekLs
                then putStrLn "c. Nincsenek termekek."
                else do
                    let elofordulas = map (\ls -> (head ls, length ls))
                            . group . sort $ map tNev termekekLs
                        (legNepTNev, legNepSz) = maximumBy (comparing snd) elofordulas
                    putStrLn $ "c. Legnepszerubb termek: "
                        ++ legNepTNev ++ " (" ++ show legNepSz ++ "x)"

fel11 :: [Rendeles] -> IO ()
fel11 rLs = do
    let varosok = nub $ map (varosnev . rcim) rLs
    putStrLn $ "Elerheto varosok: " ++ intercalate ", " varosok
    putStrLn "Add meg a keresett varos nevet:"
    v <- getLine
    varosKereső v rLs

-- ===========================================================================
-- 12. feladat – Menüvezérelt program
-- ===========================================================================

menu :: IO ()
menu = do
    putStrLn "\n============================================"
    putStrLn "1 - Rendelesek listazasa"
    putStrLn "2 - Varosi statisztika"
    putStrLn "3 - Termek statisztika"
    putStrLn "4 - Nagy rendelesek exportalasa"
    putStrLn "5 - Vasarlo statisztika"
    putStrLn "6 - Termek bevetelek"
    putStrLn "7 - Rendelesek ertektartomanyok szerint"
    putStrLn "8 - Varos atlagos termekszama"
    putStrLn "9 - Top vasarlok CSV"
    putStrLn "10 - Becsult termek bevetelek"
    putStrLn "11 - Varos kereses"
    putStrLn "0 - Kilepes"
    putStrLn "============================================"
    putStr "Valasztas: "

menuCiklus :: [Rendeles] -> String -> IO ()
menuCiklus rendelesek path = do
    menu
    valasztas <- getLine
    case valasztas of
        "0" -> putStrLn "Viszlat!"
        "1" -> do
            putStrLn "\nRendelesek (id - megrendelo - vegosszeg):"
            mapM_ (\r -> putStrLn $ show (rId r) ++ " - " ++ rNev r ++ " - " ++ show (osszeg r)) rendelesek
            menuCiklus rendelesek path
        "2" -> fel4 rendelesek path >> menuCiklus rendelesek path
        "3" -> fel3 rendelesek       >> menuCiklus rendelesek path
        "4" -> fel5 rendelesek path  >> menuCiklus rendelesek path
        "5" -> fel2 rendelesek       >> menuCiklus rendelesek path
        "6" -> fel6 rendelesek path  >> menuCiklus rendelesek path
        "7" -> fel7 rendelesek       >> menuCiklus rendelesek path
        "8" -> fel8 rendelesek       >> menuCiklus rendelesek path
        "9" -> fel9 rendelesek path  >> menuCiklus rendelesek path
        "10"-> fel10 rendelesek      >> menuCiklus rendelesek path
        "11"-> fel11 rendelesek      >> menuCiklus rendelesek path
        _   -> do
            putStrLn "Ervenytelen valasztas, probald ujra."
            menuCiklus rendelesek path

-- ===========================================================================
-- Main
-- ===========================================================================

main :: IO ()
main = do
    let path = "vizsgaparcialis_pelda_feladatok/"
    tartalom <- readFile (path <> "rendelesek.txt")
    if null tartalom
        then putStrLn "Hiba: ures fajl!"
        else do
            -- mapMaybe: csak a sikeresen értelmezett sorokat tartja meg
            let rendelesek = mapMaybe parseSor (lines tartalom)
            if null rendelesek
                then putStrLn "Hiba: egyetlen sor sem ertelmezhetoe helyesen!"
                else menuCiklus rendelesek path
