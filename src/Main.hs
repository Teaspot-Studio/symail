{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.SymReg
import Data.Genetics 

-- | Four dimentional input
sample :: ASTData -- [([Double], Double)]
sample = [ ( [fromIntegral x1, fromIntegral x2, fromIntegral x3, fromIntegral x4]
  , fromIntegral $ x1 + x2 + x3 + x4) | 
  x1 <- [0 .. 10], 
  x2 <- [0 .. 10], 
  x3 <- [0 .. 10], 
  x4 <- [0 .. 10] ]


opts :: EvOptions AST
opts = EvOptions {
    ePopSize = 20
  , eMaxGen = 100000
  , eMutaRate = 0.3
  , eElites = 0.1
  , eTarget = 1000
  , eGMC = ASTGenOpts {
      goMaxDepth = 5 -- Глубина дерева
    , goVarCount = 4 -- Количество переменных входных
    , goConstRange = (-10, 10)
    , goCVF = (1, 1, 1) -- Веса генерации констант, переменных, функций
    }
  }

main :: IO ()
main = do
  (finalPop :: Population AST) <- evolve opts sample  -- Провести эволюцию (либо макс поколение, либо достигли target ошибки)
  let (solution, fit) = getBestFit sample finalPop 
  putStrLn $ "Solution: " ++ show (showFunction solution)
  putStrLn $ "Fitness: " ++ show fit
