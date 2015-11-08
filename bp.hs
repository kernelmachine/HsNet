module BP where


import Neuron
import Layer 
import Network 
import System.Random 
import Numeric.LinearAlgebra
import Data.List
import Numeric.LinearAlgebra.Util
import Utilities

-- Backpropagation algorithm 

backProp :: LearningRate -> [[Neuron]] -> ([Double], [Double]) -> [[Neuron]]
backProp alpha nss (xs, ys) = [aux (head nss) ds_hidden xs
                        ,aux (nss !! 1) ds_out output_hidden]
    where 
      output_hidden = computeLayer (head nss) xs
      output_out = computeLayer (nss !! 1) output_hidden
      ds_out = zipWith (\s y -> s * (1 - s) * (y - s)) output_out ys
      ds_hidden = zipWith (\x s -> x * (1-x) * s) output_hidden $ map (sum . zipWith (*) ds_out) . transpose . map (weights) $ (nss !! 1)
      aux ns ds xs = zipWith (\n d -> n { weights = zipWith (\w x -> w + alpha * d * x) (weights n) xs }) ns ds


trainAuxBackProp :: Double -> [[Neuron]] -> [([Double], [Double])]  -> [[Neuron]]
trainAuxBackProp alpha = foldl' (backProp alpha)

trainBackProp :: Double -> Double -> [[Neuron]] -> [([Double], [Double])]  -> [[Neuron]]
trainBackProp alpha epsilon nss samples = until (\nss' -> globalQuadErrorNet nss' samples < epsilon) (\nss' -> trainAuxBackProp alpha nss' samples) nss
