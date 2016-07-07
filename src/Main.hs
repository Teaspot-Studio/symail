{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.SymReg
import Data.Genetics 
import Data.Ord 
import Data.List 

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
  , eMaxGen = 1000
  , eMutaRate = 0.3
  , eElites = 0.2
  , eTarget = 1000
  , eGMC = ASTGenOpts {
      goMaxDepth = 5 -- Глубина дерева
    , goVarCount = 4 -- Количество переменных входных
    , goConstRange = (-10, 10)
    , goCVF = (1, 1, 1) -- Веса генерации констант, переменных, функций
    }
  }

getBestFit' :: ASTData -> Population AST -> (AST, Double)
getBestFit' datum pop = head $ sortBy (comparing $ Down . snd) $ (\i -> (i, fitness datum i)) <$> pop 

myEvolve opts datum = do 
  pop <- randomPopulation opts
  go (eMaxGen opts) pop
  where 
  go 0 pop = do 
    let (solution, fit) = getBestFit' sample pop 
    putStrLn $ "Solution: " ++ show (showFunction solution)
    putStrLn $ "Fitness: " ++ show fit
  go n pop = do
    putStrLn $ "Generation " ++ show (eMaxGen opts - n)
    let (solution, fit) = getBestFit' sample pop 
    putStrLn $ "Solution: " ++ show (showFunction solution)
    putStrLn $ "Fitness: " ++ show fit
    pop' <- oneStep opts datum pop
    go (n-1) pop'

main :: IO ()
main = myEvolve opts sample
