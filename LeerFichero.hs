module LeerFichero (newsItems, stopW, numArchivos) where

import Clases
import ManejoString

import System.IO
import System.Directory
import System.IO.Unsafe	
import Data.List

--Recibe un directorio
directorioIO :: String -> IO [String] 
directorioIO dir = do	
	direc <- getDirectoryContents dir --Devuelve el contenido del directorio
	let salida = (delete "." (delete ".." direc))
	--Al usar linux, las carpetas tienen estos 2 archivos por defecto
	return salida

directorio :: [String]
directorio = unsafePerformIO (directorioIO "./newsCorpusUTF8")
--Hay q pasar el directorio aqui
--Transforma la salida IO[String] en un [String]

numArchivos :: Int
numArchivos = length directorio

stopWIO :: IO [String]
stopWIO = do
	file <- readFile("stopwordsUTF8.txt")
	return (words file) --Los pone en modo ["a","de", ....]

--Recupera la lista de stopwords en [String]
stopW :: [String]
stopW = unsafePerformIO (stopWIO)

--Recibe un nombre, y lo recupera como Noticia
leerNoticiaIO :: String -> IO Noticia
leerNoticiaIO fn = do
 file <- readFile( "newsCorpusUTF8/" ++ fn)
 return (crearNoticiaText (lines file))

leerNoticia :: String -> Noticia
leerNoticia fn = unsafePerformIO (leerNoticiaIO fn)
--Transforma la salida IONoticia en un Noticia

leerNoticias :: [String] -> Noticias
leerNoticias [] = []
leerNoticias (x:xs) = [leerNoticia x] ++ leerNoticias xs
--Recibe una lista de nombres de archivo, y devuelve una lista de Noticia 

newsItems :: Noticias
newsItems = leerNoticias directorio
--Formato que especifica en la practica para las llamadas

crearNoticiaText :: [String] -> Noticia
crearNoticiaText file = crearNoticia (getTitular file) (getFecha file) (getFuente file) (getCuerpo file)
--Crea una noticia desde un texto