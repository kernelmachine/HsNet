module Main where

import Neuron
import Layer 
import Network 
import System.Random 
import Numeric.LinearAlgebra
import Utilities



main = do 
	k <- randomItem [[1..100],[1..100]]
	let numberNeurons = 2
	let samples = [([1,0],[1]), ([0,1],[1]), ([1,1],[1]),([0,0],[0]),([0,1],[1]),([1,1],[1]),([0,1],[1]),([0,0],[0]), ([1,0],[1])]
	ann <- return $ createNetwork [numberNeurons,numberNeurons] k 0.2
	net <- return $ getAllLayers ann
	j <- return $ convert numberNeurons (toList $ fst $ trainLM net samples 0.01 10 0.01 1000000)
	trainedNet <- return $ updateNetwork net j
	let input1 = [1,1]
	let input2 = [0,1]
	let input3 = [1,0]
	let input4 = [0,0]
	putStr $ show $ net
	putStr  "\n"
	putStr $ show $ trainedNet 
	test1 <- return $ computeNet trainedNet input1
	test2 <- return $ computeNet trainedNet input2
	test3 <- return $ computeNet trainedNet input3
	test4 <- return $ computeNet trainedNet input4
	putStr "\n\n"
	putStr $ show $ input1 
	putStr " -> "
	putStr $ show $ test1
	putStr "\n\n"
	putStr "\n\n"
	putStr $ show $ input2 
	putStr " -> "
	putStr $ show $ test2
	putStr "\n\n"
	putStr "\n\n"
	putStr $ show $ input3
	putStr " -> " 
	putStr $ show $ test3
	putStr "\n\n"
	putStr "\n\n"
	putStr $ show $ input4 
	putStr " -> "
	putStr $ show $ test4
	putStr "\n\n"
