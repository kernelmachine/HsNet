module Neuron where

import Numeric.LinearAlgebra
import System.Random
import Numeric.LinearAlgebra.Util
import Data.List 
import Utilities

type LearningRate  = Double 
type MomentumFactor = Double
type NumberOutputNodes = Int
type NumberInputNodes = Int
type NumberHiddenNodes = Int
type Iteration = Int
type Target = Double
type Targets = [Double]
type Patterns = [([Double], Double)]
type Pattern = ([Double], Double)
type Weights = [Double]
type Inputs = [Double]
type Network = [Layer]
type Layer =  [Neuron]


data Neuron = Neuron {
	threshold :: Double, 
	weights :: [Double],
	func :: Double -> Double
}

neuronThreshold (Neuron{threshold = t })= t
neuronWeights (Neuron{weights = ws })= ws
neuronFunc (Neuron{func = f })= f

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp (-x))

instance Show Neuron where
    show n = "\n\t\tNeuron:" ++"\n\t\t\tThreshold : " ++ show (threshold n) ++ "\n\t\t\tWeights : " ++ show (weights n) 

createNeuron :: Double -> Weights -> (Double -> Double) -> Neuron
createNeuron t ws f = Neuron { threshold = t, weights = ws, func = f }

createNeuronSigmoid :: Double -> Weights -> Neuron
createNeuronSigmoid t ws = createNeuron t ws sigmoid

compute :: Neuron -> Inputs -> Double
compute n inputs | (length  inputs ) == (length $ weights n)
                     = func n $ ((fromList inputs)<.>(fromList $ weights n)) - threshold n

compute n inputs = error $ "Number of inputs != Number of weights\n" ++ show n ++ "\nInput : " ++ show inputs
learnSample :: LearningRate -> Neuron -> Pattern  ->  Neuron
learnSample l n  (xs,y) = n { 
                          threshold = threshold n
                        , weights = map_weights (weights n) (xs, y) 
                        , func = func n
                        }
    where map_weights ws (xs, y) = let s = compute n xs in
                                   zipWith (\w_i x_i -> w_i + l*(y-s)*x_i) (ws) (xs)

learnSamples :: LearningRate -> Neuron -> Patterns -> Neuron
learnSamples alpha = foldl' (learnSample alpha)
