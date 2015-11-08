module LM where


import Neuron
import Layer 
import Network 
import System.Random 
import Numeric.LinearAlgebra
import Numeric.AD
import Utilities


updateNeuron n j = n {weights = j}

updateLayer j ns = zipWith updateNeuron ns j

updateNetwork nss j = map (updateLayer j) nss


quadErrorNetVecAux nss (xs,ys) = zipWith (\y s -> 0.5 * (y - s)**2) ys $ computeNet nss xs

quadErrorNetVec nss samples = fromList $ concatMap (quadErrorNetVecAux nss) samples
    

jacobError :: [[Neuron]] -> [([Double],[Double])] -> Vector Double ->  Matrix Double 
jacobError nss samples residues = ((length ys) >< (length xs)) (concat  $ map (take 1) x )
            where 
              x = [p x (y) | x <- xs,  y <- ys]
              p x y = grad (\[x,y] -> 0.5 * (x - y)**2) [x, y] 
              ys = concatMap (take 1) $ map (computeNet nss) ps 
              ps = map (fst) samples 
              xs = concatMap (\n -> weights n) (head nss)  ++ concatMap (\n -> weights n) (nss !! 1)


-- Levenberg-Marquardt Algorithm. Doesn't work fully yet. 

lmMinimize func jacob param lambda beta prec iter samples nss =
  let errValue = globalQuadErrorNet nss samples
      (v, m) = lmMinimizeInternal func jacob param prec errValue lambda beta iter [param] nss samples
  in
   (v, (fromLists . reverse . map toList) m)


lmMinimizeInternal _ _ v _ _ _ _ 0 m _ _= (v,m)
lmMinimizeInternal residues jacobian param prec errValue lambda beta iter mat nss samples =
    let 
      gradients = (trans jacobian) `mXv` residues
      hessian = (trans jacobian) `multiply` jacobian

      diagonalHessian = diagRect 0 (takeDiag hessian) (cols hessian) (cols hessian)
      diagonalLambda = mapMatrix (*lambda) (ident (cols hessian))
      hessianModded = hessian `add` diagonalLambda

      newParam = param `sub` (flatten (linearSolveSVD hessianModded (reshape 1 gradients)))

      newErrValue = globalQuadErrorNet nss samples
      newResidues = quadErrorNetVec nss samples
      newJacobian = jacobError nss samples newResidues
  in
   case abs(newErrValue - errValue) < prec of
     True -> (newParam, newParam:mat)
     False ->
       case newErrValue > errValue of
         True  -> lmMinimizeInternal residues jacobian param prec errValue (lambda * beta) beta (iter - 1) mat nss samples 
         False -> lmMinimizeInternal newResidues newJacobian newParam prec newErrValue (lambda / beta) beta (iter - 1) (newParam:mat) nss samples

trainLM nss samples lambda beta prec iter = lmMinimize (residues) (jacobian) (param nss) lambda beta prec iter samples nss
                                where
                                  residues = quadErrorNetVec nss samples
                                  jacobian = jacobError nss samples residues
                                  param nss = fromList $ concatMap (\n -> weights n) (head nss)  ++ concatMap (\n -> weights n) (nss !! 1)
                                 