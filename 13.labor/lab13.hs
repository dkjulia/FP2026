{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lab13 where

-- ghci-n belul :set -package aeson
import           Data.Aeson
-- ghci-n belul :set -package bytestring
import qualified Data.ByteString.Lazy as B
import           Data.Char
import           Data.List
import           Data.Ord             (comparing)
import           GHC.Generics
-- # 13. labor

-- I. A tudosok.json állomány JSON szerkezetű, tudósok adatait tárolja: vezetéknév, nemzetiség, születési év és elhalálozási év. Egy ilyen szerkezetű állomány tartalma a következő lehet:

-- ```json
-- {"tudosok":[
--   {"nev" :"Euler",
--   "nemzetiseg" :"svajc",
--   "szEv" :1707,
--   "hEv" :1783
--   },
--   {"nev" :"Bolyai Janos",
--   "nemzetiseg" :"magyar",
--   "szEv" :1802,
--   "hEv" :1860
--   },
--   {"nev" :"Perelman",
--   "nemzetiseg" :"orosz",
--   "szEv" :1966
--   }
-- ]}
-- ```

-- - Írjunk egy Haskell programot, amely
-- - az állományban levő adatok alapján létrehoz egy Tudosok adatszerkezetet a következő két adatszerkezetet használva:

data Tudos = Tudos {
  nev        :: String,
  nemzetiseg :: String,
  szEv       :: Int,
  hEv        :: Maybe Int
} deriving (Show, Read, Generic, FromJSON, ToJSON)

newtype Tudosok = Tudosok {
  tudosok :: [Tudos]
} deriving (Show, Read, Generic, FromJSON, ToJSON)

beolvas :: FilePath -> IO (Maybe Tudosok)
beolvas fajl = do
    content <- B.readFile fajl
    return (decode content)

-- 1. meghatározza a tudósok születési év szerint rendezett sorrendjét,
rendezEv :: [Tudos] -> [Tudos]
rendezEv ts = sortOn szEv ts

-- 2. meghatározza a tudósok életkorát és abban az esetben, ha nem jelenik meg egy tudósnál elhalálozási év, az életkor helyett a kortars szót tünteti fel,
eletkor :: Tudos -> String
eletkor t =
    case hEv t of
        Nothing -> "kortars"
        Just h  -> show (h - szEv t)

-- 3. meghatározza a tudósok életkor szerinti rendezett sorrendjét,
eletkorSzam :: Tudos -> Int
eletkorSzam t =
    case hEv t of
        Nothing -> maxBound :: Int
        Just h  -> h - szEv t

rendezEletkor :: [Tudos] -> [Tudos]
rendezEletkor ts = sortOn eletkorSzam ts
-- 4. egy állományba kiírja JSON formában adott nemzetiségű tudósok listáját.
szurNemzetiseg :: String -> [Tudos] -> [Tudos]
szurNemzetiseg n =
    filter (\t -> nemzetiseg t == n)

kiirNemzetiseg :: FilePath -> String -> [Tudos] -> IO ()
kiirNemzetiseg fajl n ts = do
    let filtered = Tudosok (szurNemzetiseg n ts)
    B.writeFile fajl (encode filtered)

kiirTudos :: Tudos -> IO ()
kiirTudos t =
    putStrLn (nev t ++ " "
    ++ nemzetiseg t ++ " "
    ++ show (szEv t) ++ " " ++
    maybe "-" show (hEv t))

mainI :: IO ()
mainI = do
    tartalom <- B.readFile "13.labor/tudosok.json"
    let eredmeny = decode tartalom :: Maybe Tudosok

    -- vagy
    -- eredmeny <- beolvas "13.labor/tudosok.json"

    case eredmeny of
        Nothing           -> putStrLn "Hibas JSON!"
        Just (Tudosok ts) -> do
            -- 1.
            putStrLn "Szuletesi ev szerint rendezve"
            mapM_ kiirTudos (rendezEv ts)

            -- 2.
            putStrLn "\nEletkorok: "
            mapM_ (\t -> putStrLn (nev t ++ " " ++ eletkor t)) ts

            -- 3.
            putStrLn "\nEletkor szerint rendezve"
            mapM_ kiirTudos (rendezEletkor ts)

            -- 4.
            putStrLn "\nKiiratas JSON-be"
            let targetNemzetiseg = "magyar"
            let nemzetiseguek = Tudosok (filter (\t -> nemzetiseg t == targetNemzetiseg) ts)

            B.writeFile "13.labor/nemzetiseguek.json" (encode nemzetiseguek)
            -- vagy
            -- kiirNemzetiseg "13.labor/nemzetiseguek.json" targetNemzetiseg ts
            putStrLn "\nA kiiratas befejezodott"

-- II. Az autok.json állomány JSON szerkezetű, személygépkocsik adatait tárolja: gyártmány (String), modell (String), évjárat (Int). Írjunk egy Haskell-programot, amely
data Auto = Auto {
    gyartmany :: String,
    modell    :: String,
    evjarat   :: Int
} deriving (Show, Read, Generic, FromJSON, ToJSON)

newtype Autok = Autok {
  autok :: [Auto]
} deriving (Show, Read, Generic, FromJSON, ToJSON)

-- 1. kiírja a képernyőre a személygépkocsik adatait, az évjárat szerinti mező alapján rendezve, minden sorba egy gyártmány, modell, illetve évjárat értéket írva,
-- 2. létrehoz egy gyartmany.json JSON formátumú állományt, amelybe átírja megadott gyártmányú személygépkocsik adatait, pontosabban a modell és évjárat értékeket, ahol a keresett gyártmány értékét a billentyűzetről olvassuk be,
-- 3. létrehoz egy autokJavitva.json JSON formátumú állományt, amelybe a személygépkocsik adatait úgy írja át, hogy minden gyártmánynév, illetve modellnév esetében ha kisbetűvel kezdődik, akkor a kezdőbetűt átalakítja nagybetűvé.
nagykezdobetusit :: [Char] -> [Char]
nagykezdobetusit []     = []
nagykezdobetusit (x:xs) = toUpper x : xs

javitAuto :: Auto -> Auto
javitAuto a =
    a {
        gyartmany = nagykezdobetusit (gyartmany a),
        modell = nagykezdobetusit (modell a)
    }

kiirAuto :: Auto -> IO ()
kiirAuto a = putStrLn (gyartmany a ++ " " ++ modell a ++ " " ++ show (evjarat a))

mainII :: IO ()
mainII = do
    tartalom <- B.readFile "13.labor/autok.json"
    let eredmeny = decode tartalom :: Maybe Autok

    case eredmeny of
        Nothing -> putStrLn "Hibas JSON!"
        Just (Autok aLs) -> do
            -- 1.
            putStrLn "Evjarat szerint rendezve"
            mapM_ kiirAuto (sortOn evjarat aLs)

            -- 2.
            putStrLn "Adj meg egy gyartmanyt: "
            targetGyartmany <- getLine
            let gyartmanyLs = filter (\a -> gyartmany a == targetGyartmany) aLs
            let gyartmanyJson = map (\a -> object ["modell" .= modell a,
                                                   "evjarat" .= evjarat a]) gyartmanyLs
            B.writeFile "13.labor/gyartmany.json" (encode gyartmanyJson)
            -- 3.
            let javitott = map javitAuto aLs
            B.writeFile "13.labor/autokJavitva.json" (encode javitott)


-- III. A betegek.json állomány JSON szerkezetű, betegek adatait tárolja: név (String), ország (String), születési év (Int), betegségek ([String]). Írjunk egy Haskell-programot, amely feldolgozza az állományban levő adatokat és
data Beteg = Beteg {
    bNev         :: String,
    bOrszag      :: String,
    bSzuletesiEv :: Int,
    bBetegsegek  :: [String]
} deriving (Show, Read, Generic, FromJSON, ToJSON)

newtype Betegek = Betegek {
  betegek :: [Beteg]
} deriving (Show, Read, Generic, FromJSON, ToJSON)

-- 1. kiírja a képernyőre egy adott országon belül a betegségeket és a betegségek számát, ahol az országnevet a billentyűzetről olvassuk be,
-- 2. meghatározza, hogy melyik országban van a legtöbb fajta betegség,
-- 3. létrehoz egy orszag.json JSON formátumú állományt, amelybe átírja megadott országú betegek adatait, pontosabban a nevet, születési évet és a betegségeket, ahol a keresett ország nevét a billentyűzetről olvassuk be.
kiirBeteg :: Beteg -> IO ()
kiirBeteg b = putStrLn (bNev b ++ " " ++ show (bSzuletesiEv b) ++ " " ++ bOrszag b ++ " " ++ intercalate ", " (bBetegsegek b))

mainIII :: IO ()
mainIII = do
    tartalom <- B.readFile "13.labor/betegek.json"
    let eredmeny = decode tartalom :: Maybe Betegek

    case eredmeny of
        Nothing -> putStrLn "Hibas JSON!"
        Just (Betegek bLs) -> do
            -- mapM_ kiirBeteg bLs
            -- 1.
            putStrLn "Adj meg egy orszagot:"
            orszag <- getLine
            let orszagBetegsegek = concat [bBetegsegek b | b <- bLs, bOrszag b == orszag]
            putStrLn (orszag ++ " betegsegei" ++ " (" ++ show (length orszagBetegsegek) ++ ")" ++ ": " ++ intercalate ", " orszagBetegsegek)

            -- 2.
            let orszagok = nub (map bOrszag bLs)
            -- nub - eltorli a duplikatumokat a listabol, egy elem csak egyszer fog szerepelni
            let legtobbBetegseg = maximumBy (comparing snd) [(o,  (length . nub) (concatMap bBetegsegek (filter (\b -> bOrszag b == o) bLs))) | o <- orszagok]
            putStrLn ("A legtobb fajta betegseg orszaga " ++ fst legtobbBetegseg ++ ", szama " ++ show (snd legtobbBetegseg))

            -- 3.
            let orszagBetegek = filter (\b -> bOrszag b == orszag) bLs
            let orszagBetegekJson = map (\b -> object ["nev" .= bNev b,
                                                        "szuletesiEv" .= bSzuletesiEv b,
                                                        "betegsegek" .= bBetegsegek b]) orszagBetegek
            B.writeFile "13.labor/orszag.json" (encode orszagBetegekJson)