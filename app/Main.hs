module Main where

import System.Environment (getProgName)
import System.Exit (die)
import System.FilePath (takeFileName)

import qualified Lab7
import qualified Lab8
import qualified Lab9 
import qualified Lab13
import qualified Feladatok2 

main :: IO ()
main = do
  progName <- takeFileName <$> getProgName
  case progName of
    "lab7" -> Lab7.main1
    "lab8" -> Lab8.mainI
    "lab9" -> Lab9.mainI
    "lab13" -> Lab13.mainI
    "feladatok2" -> Feladatok2.mainI 
    _       -> die $ "Unknown executable: " ++ progName ++ ". Use `cabal run lab7` or `cabal run lab8`."