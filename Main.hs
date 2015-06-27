module Main where

import LeerFichero
import ManejoString
import Clases
import OrthographicMeasures

import Data.List
import Data.Char

--Las funciones pedidas estan hechas en 2 partes para hacerlo ampliable, en una recupera la informacion pedida
--y en su funcion "show*" lo muestra por pantalla con el formato pedido.

--Muestra Fuentes
showNewsPapers :: Noticias -> IO()
showNewsPapers lis = do
	mapM_ putStrLn (newspapers lis) 

--Devuelve Lista de Fuentes
newspapers :: Noticias -> [Fuente]
newspapers lis = nub(newspapersAux lis)

newspapersAux :: Noticias -> [Fuente]
newspapersAux [] = []
newspapersAux (x:xs) = [getFuenteNoti x] ++ newspapersAux xs 
--Recupero todas las fuentes (duplicadas) y en la llamada a este, borro las duplicadas con nub

--Muestra Titulares publicados por una Fuente
showTitlesBySource :: Noticias -> String -> IO()
showTitlesBySource lis f = do
	let noticias =  [getTitularNoti x| x <- (noticiasBySource lis f) ]
	--Listas de comprension para guardar el string de la salida pedida y mostrarlo con formato
	mapM_ putStrLn noticias  

--Muestra los Titulos de las Noticias q tengan un numero de parrafos de Cuerpo mayor o igual a i
showTitlesByParagraphs :: Noticias -> Int -> IO()
showTitlesByParagraphs  lis i = do
	mapM_ putStrLn (titlesByParagraphs lis i)

--Devuelve los Titulos de las Noticias q tengan un numero de parrafos de cuerpo mayor o igual a i
titlesByParagraphs :: Noticias -> Int -> [Titular]
titlesByParagraphs [] _ = []
titlesByParagraphs (x:xs) i = if (length (borrarLineasBlanco(getCuerpoNoti x)) >= i ) then [getTitularNoti x] ++ titlesByParagraphs xs i
									else titlesByParagraphs xs i

--Muestra las Noticias que tengan la cadena en su Titular
showNewsBySearch :: Noticias -> String -> IO()
showNewsBySearch lis cad = do
	putStrLn "------------------------"
	let noticias =  [getTitularNoti x ++"\n"++ getFuenteNoti x ++ " - " ++ show (getFechaNoti x) ++ "\n"++ unlines(getCuerpoNoti x) ++ "\n------------------------\n"| x <- (newsBySearch lis cad) ]
	--Listas de comprension para guardar el string de la salida pedida y mostrarlo con formato
	mapM_ putStrLn noticias

--Devuelve las Noticias que tengan la cadena en su Titular
newsBySearch :: Noticias -> String -> Noticias
newsBySearch [] _ = []
newsBySearch _ [] = []
newsBySearch (x:xs) cad = if (isInfixOf cad (getTitularNoti x)) then [x] ++ newsBySearch xs cad
									else newsBySearch xs cad

--Muesta las Noticias ordenadas, 
showNewsOrd :: Noticias -> IO()
showNewsOrd lis = do
	--Noticia implementa Ord, solo tienes q decirle q ordene la lista
	putStrLn "------------------------"
	let noticias =  [ show x ++ "\n------------------------\n"| x <- (sort lis) ]
	--Listas de comprension para guardar el string de la salida pedida y mostrarlo con formato
	mapM_ putStrLn noticias	

--Muestra las entidades de cada Noticia de una Fuente
showEntiBySource :: Noticias -> String -> IO()
showEntiBySource lis f= do 
	putStrLn "------------------------"
	let noticias =  [getTitularNoti x ++ "\n" ++"\nEntidades :\n\n"++ unlines(nub(entidades x stopW)) ++ "\n------------------------\n"| x <-(noticiasBySource lis f)]
	--Listas de comprension para guardar el string de la salida pedida y mostrarlo con formato
	mapM_ putStrLn noticias

--Devuelve las Noticias publicadas por una Fuente
noticiasBySource :: Noticias -> String -> Noticias
noticiasBySource [] _ = []
noticiasBySource _ [] = []
noticiasBySource (x:xs) f = if ((getFuenteNoti x) == f ) then [x] ++ noticiasBySource xs f
								else noticiasBySource xs f 

--Muestra los grupos de Noticia "ordenados" por tema
showGroups :: Noticias -> [String] -> IO ()
showGroups lis sw = do
	putStrLn "------------------------"
	let titulares = [ mostrarTitulares x ++ " -------------" | x <- (groupNot lis sw)] 
	mapM_ putStrLn titulares

--Para hacer grupos vamos cogiendo de la lista de Noticia, el primer elemento siempre, de ahi sacamos sus noticias similares
--ahora esas noticias similares hacemos "difference" con la lista de Noticia, lo q nos borrara las noticias q ya han aparecido
--y volver a llamar con el primer elemento (al borrar elementos de Noticia, ha cambiado)
groupNot :: Noticias -> [String] -> [Noticias]
groupNot lis sw = if (lis /= []) then [groupNotAux lis (getI lis 0) sw] ++ groupNot (lis\\(groupNotAux lis (getI lis 0) sw)) sw
										else [[]]

--Dada una lista de Noticia y una Noticia, devuelve las similares .....  Para probar con el showGroups2
groupNotAux :: Noticias -> Noticia -> [String] -> Noticias
groupNotAux [] _ _ = []
groupNotAux _ _ [] = []
groupNotAux (x:xs) n sw = if (similars (unlines(borrarStopW x sw)) (unlines(borrarStopW n sw)) 29) then [x] ++ groupNotAux xs n sw
							else groupNotAux xs n sw

--Con una noticia compara y sale el grupo "(getI lis 0)" -- Esta de prueba
--showGroups2 :: Noticias -> [String] -> IO()
--showGroups2 lis sw = do
--	--para imprimir
--	let t = [getTitularNoti x | x <- (groupNotAux lis (getI lis 5) sw)] 
--	mapM_ putStrLn t

mostrarTitulares :: Noticias -> String
mostrarTitulares [] = []
mostrarTitulares (x:xs) = getTitularNoti x ++ "\n" ++ mostrarTitulares xs
--Le llega una lista de Noticia, y devuelve sus Titulares en forma de String