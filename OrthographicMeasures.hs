module OrthographicMeasures (normalizedDistance,toLowerCase,similars) where

import Data.Char

distance :: Eq a => [a] -> [a] -> Int
distance a b 
    = last (if lab == 0 then mainDiag
	    else if lab > 0 then lowers !! (lab - 1)
		 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
	  lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z
          
-- Distancia de Edición Normalizada          
normalizedDistance ::  Eq a => [a] -> [a] -> Float
normalizedDistance cad1 cad2 = (fromIntegral (distance cad1 cad2))/(fromIntegral value)
									where value = max (length cad1) (length cad2)
									
																
-- Dado un porcentaje devuelve un umbral 
getThreshold :: Integer -> Float
getThreshold p = (fromInteger p)/100 

-- Convierte una cadena a minúsculas
toLowerCase :: String -> String
toLowerCase [] = []
toLowerCase (x:xs) = toLower x:toLowerCase xs

-- Dice si dos cadenas son similares con un determinado porcentaje
-- El tercer parámetro es un valor entre 0 y 100.
similars :: String -> String -> Integer -> Bool
similars c1 c2 p = (1-normalizedDistance (toLowerCase c1) (toLowerCase c2)) > getThreshold p 									 