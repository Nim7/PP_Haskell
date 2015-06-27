module ManejoString (getI, getTitular,getFecha,getFuente,getCuerpo,borrarLineasBlanco, entidades, borrarStopW) where

import OrthographicMeasures
import Clases

import Data.Char
import Data.List

entidades :: Noticia -> [String] -> [String]
entidades n sw = entidadesAux (borrarStopW n sw) 
--Devuelve la lista de "entidades" de una Noticia

entidadesAux :: [String] -> [String]
entidadesAux [] = []
entidadesAux (x:xs) = if (isUpper(getI x 0)) then [x] ++ entidadesAux xs
						else entidadesAux xs
--Devuelve las palabras q empiezan por Mayuscula

borrarStopW :: Noticia -> [String] -> [String]
borrarStopW n sw = nub(borrarStopWAux1 (noticiaSinMayus n) sw)
--Borra los stopWords de una Noticia q ya "no tiene Mayusculas" (q sea primera de linea o despues de punto) 
--y borra los duplicados, por si una notia es mas larga q otra y hablan de lo mismo, seguramente se repitan palabras importantes
--ademas de reducir el tiempo de procesamiento

borrarStopWAux1 :: [String] -> [String] -> [String]
borrarStopWAux1 [] _ = []
borrarStopWAux1 (x:xs) sw = borrarStopWAux (words x) sw ++ borrarStopWAux1 xs sw 
--Por cada linea de String, llama con words al siguiente auxiliar para borrar stopWords

borrarStopWAux :: [String] -> [String] -> [String]
borrarStopWAux lis [] = lis
borrarStopWAux lis (x:xs) = borrarStopWAux (deleteAllInstances x lis ) xs
--Borra todas las stopWords de una lista de String

noticiaSinMayus :: Noticia -> [String]
noticiaSinMayus  n = borrarMayus(borrarLineasBlanco([getTitularNoti n] ++ getCuerpoNoti n))
--Borra las palabras iniciales de linea y las de despues de los puntos y lo devuelve junto
--Puede borrar alguna "entidad", pero en mi opinion, creo q borra mas stopWords q "entidades"

borrarMayus :: [String] -> [String]
borrarMayus [] = []
borrarMayus (x:xs) = [borrarMayus' x] ++ borrarMayus xs
--Borrar las mayusculas despues de '.' o inicio de linea

borrarMayus' :: String -> String
borrarMayus' cad = concat( addEspacioFinal(borrarMayusAux(split cad '.')))
--Separo por '.' para q borre las primeras, inserto un espacio al final
--y vuelvo a concatenar

addEspacioFinal :: [String] -> [String]
addEspacioFinal [] = []
addEspacioFinal (x:xs) = [x ++ " "] ++ addEspacioFinal xs
--Añade un espacio al final de las lineas para cuando se concatenen

borrarMayusAux :: [String] -> [String]
borrarMayusAux [] = []
borrarMayusAux (x:xs) = if ((words x)/=[]) then [unwords(tail(words x))] ++ borrarMayusAux xs
							else borrarMayusAux xs 
--Borrar las palabras en mayusculas q sean o la primera o despues de punto
--Normalmente la primera palabra no tiene significado
--El if esta para evitar excepcion si haces tail de [] 

deleteAllInstances :: Eq a => a -> [a] -> [a]
deleteAllInstances a (x:xs)
    | a == x    = rest
    | otherwise = x : rest
      where
        rest = deleteAllInstances a xs
deleteAllInstances _ _ = []
--http://stackoverflow.com/questions/10114228/delete-all-instances-from-list-haskell

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim
--http://julipedia.meroh.net/2006/08/split-function-in-haskell.html
--funcionamiento del split tipico de otros lenguajes

borrarLineasBlanco :: [String] -> [String]
borrarLineasBlanco [] = []
borrarLineasBlanco (x:xs) = if (x == "\n") then borrarLineasBlanco xs
								else [x] ++ borrarLineasBlanco xs
--Borra lineas en blanco de una lista de Strings

getI :: [a] -> Int -> a
getI lis i = lis!!i
--Devolver la linea de una lista, valida para cualquier tipo de lista

getTitular :: [String] -> String
getTitular lis = getI lis 0
--Coge la primera linea y ya

getFuente :: [String] -> String
getFuente lis = tail(getI (split (getI lis 1) ',') 0)
--Coge la 2 linea del txt, parte por la coma
--se queda con la primera parte y quita "<" inicial

getFecha :: [String] -> Fecha
getFecha lis = getFechaAux (parseIntList(split (delete '\r' (delete '>' (getI (split (getI lis 1) ',') 1))) '/'))
--Coge la 2 linea del txt, parte por la coma
--quedarte con la 2 parte, le quita el ">" final y el "\r" final
--Lo q queda lo parte por '\' y lo transforma a Int

getFechaAux :: [Int] -> Fecha
getFechaAux lis = crearFecha (getI lis 0) (getI lis 1) (getI lis 2) 
--A partir de una lista de Int, crea una Fecha

getCuerpo :: [String] -> [String]
getCuerpo lis =  tail(tail lis)
--Eliminamos los 2 primeros parrafos y listo

parseIntList :: [String] -> [Int]
parseIntList [] = []
parseIntList (x:xs) = [parseInt x] ++ parseIntList xs
--Transforma una lista de String a lista de Int

parseInt :: String -> Int
parseInt s = parseIntAux (reverse s) 0 
--Da la vuelta al String para q el orden sean: unidades,decenas...

parseIntAux :: String -> Int -> Int
parseIntAux [] _ = 0
parseIntAux (x:xs) i = (digitToInt x)*(10^i) + parseIntAux xs (i+1)
--1 char, lo transformas a entero, y lo multiplicas por 10^0 (= 1) unidades
--2 char, lo transformas a entero, y lo multiplicas por 10^1 (= 10) decenas
-- .... los q sean y los sumas
