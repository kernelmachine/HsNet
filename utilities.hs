module Utilities where

import Numeric.LinearAlgebra
import System.Random
import Numeric.LinearAlgebra.Util
import Neuron
import Layer
import Data.List 
import Numeric.AD
import Utilities


-- Random utility functions for network operation


convert :: Int -> [a] -> [[a]]
convert _ [] = []
convert k (x:xs) = take k (x:xs) : convert k (drop k (x:xs))

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0
   where addDigit num d = num + d

randomItem :: [a] -> IO a
randomItem xs = fmap (xs!!) $ randomRIO (0, length xs - 1)

inits' :: [a] -> [[a]]
inits' [] = []
inits' (x:xs) = ((inits (x:xs) )!! 1):(inits' xs)

randomWeights :: Int -> Int -> [Double]
randomWeights 0 _= [0]
randomWeights n k = toList $ randomVector k Uniform n

mapRandomWeights :: [Int] -> [Int] -> [[Double]]
mapRandomWeights n k = zipWith (\n' k' -> randomWeights n' k') n k

createNeuralLayer n k t= zipWith (\n' k' -> createSigmoidLayer (fromDigits n') t k') (inits' n) (mapRandomWeights n k)

check :: [[Neuron]]-> Bool
check nss = let l = length (nss) in l > 1 && l < 3

nn :: [[Neuron]] -> [[Neuron]]
nn nss | check (nss) = nss
       | otherwise = error "Invalid nn"
