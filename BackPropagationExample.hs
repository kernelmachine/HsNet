module Main where

import Neuron
import Layer 
import Network 
import System.Random 
import Numeric.LinearAlgebra
import Utilities


main = do 
	k <- randomItem [[1..100],[1..100]]
	ann <- return $ createNetwork [2,2] k 0.2
	net <- return $ getAllLayers ann
	j <- return $ trainBackProp 0.5 0.1 net [([0,1],[0]), ([0,1],[0]), ([0,1],[0]), ([0,1],[0]), ([1,1],[1]), ([1,1],[1]), ([1,1],[1]), ([1,1],[1]), ([1,1],[1])]
	let input1 = [1,1]
	let input2 = [0,1]
	let input3 = [1,0]
	let input4 = [0,0]
	test1 <- return $ computeNet j input1
	test2 <- return $ computeNet j input2
	test3 <- return $ computeNet j input3
	test4 <- return $ computeNet j input4
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