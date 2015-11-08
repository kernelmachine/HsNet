module Layer where

import Neuron
import Numeric.LinearAlgebra
import System.Random
import Numeric.LinearAlgebra.Util
import Data.List 
import Utilities

data NeuralLayer = NeuralLayer{
	layer :: [Neuron]
}



instance Show NeuralLayer where
    show n = "\n\tLayer : " ++ (show $ layer n) ++ "\n" 

createSigmoidLayer :: Int -> Double -> [Double] -> NeuralLayer
createSigmoidLayer n threshold weights = NeuralLayer {
			layer = take n . repeat $ createNeuronSigmoid threshold weights
}
             

computeLayer :: [Neuron] -> Inputs -> [Double]
computeLayer ns inputs = map (\n -> compute n inputs) (ns)

learnSampleLayer :: Double -> [Neuron] -> ([Double], [Double]) -> [Neuron]
learnSampleLayer alpha ns (xs, ys) = zipWith (\n y -> learnSample alpha n (xs, y)) ns ys

learnSamplesLayer :: Double -> [Neuron] -> [([Double], [Double])] -> [Neuron]
learnSamplesLayer alpha = foldl' (learnSampleLayer alpha)

quadError :: [Neuron] -> ([Double], [Double])  -> Double
quadError ns (xs, ys) = let os = computeLayer ns xs
                        in (/2) $  sum $ zipWith (\o y -> (y - o)**2) os ys
