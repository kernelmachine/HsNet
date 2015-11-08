module Network where


import Numeric.LinearAlgebra
import System.Random
import Numeric.LinearAlgebra.Util
import Neuron
import Layer
import Data.List 
import Numeric.AD
import Utilities

data NeuralNetwork = NeuralNetwork{
	network :: [NeuralLayer]
}

instance Show NeuralNetwork where
    show n = "Network : " ++ (show $ network n) ++ "\n"

-- Useful functions for retreiving network content 

unpackNetwork :: NeuralNetwork -> [NeuralLayer]
unpackNetwork (NeuralNetwork {network = n}) = n

unpackLayer :: NeuralLayer -> [Neuron]
unpackLayer (NeuralLayer {layer = n}) = n

getLayer :: Int -> NeuralNetwork -> [Neuron]
getLayer k network = unpackLayer $ (unpackNetwork network) !! k

getAllLayers :: NeuralNetwork -> [[Neuron]]
getAllLayers network = map (unpackLayer) (unpackNetwork network)

getNeuron :: Int -> Int -> NeuralNetwork -> Neuron
getNeuron k p network = (getLayer p network) !! k

getNeuronThreshold :: Int -> Int -> NeuralNetwork -> Double
getNeuronThreshold k p network = neuronThreshold $ getNeuron k p network

getNeuronWeights :: Int -> Int -> NeuralNetwork -> [Double]
getNeuronWeights  k p network = neuronWeights $ getNeuron k p network


-- Network Functions

createNetwork :: [Int] -> [Int] -> Double ->  NeuralNetwork 
createNetwork n k t = NeuralNetwork {
				network = createNeuralLayer n k t

				}


computeNet :: [[Neuron]] -> Inputs -> [Double]
computeNet neuralss xs = let nss = nn neuralss in computeLayer (nss !! 1) $ computeLayer (head nss) xs
  

quadErrorNet :: [[Neuron]] -> ([Double], [Double]) -> Double
quadErrorNet nss (xs,ys) = (sum . zipWith (\y s -> (y - s)) ys $ computeNet nss xs)/2.0


globalQuadErrorNet :: [[Neuron]] -> [([Double], [Double])] -> Double
globalQuadErrorNet nss = sum . map (quadErrorNet nss)


          