module Clases where

data Fecha = F Int Int Int deriving Eq

instance Show Fecha where
    show (F d m a)=show d ++ "/" ++ show m ++ "/" ++ show a
    
--instance Eq Fecha where
	--(F d1 m1 a1) == (F d2 m2 a2) = d1==d2 && m1==m2 && a1==a2
-- Con el deriving Eq no hace falta .... creo

instance Ord Fecha where
    (F d1 m1 a1) < (F d2 m2 a2) = d1<d2 && m1==m2 && a1==a2
                                || m1<m2 && a1==a2
                                || a1<a2
    (F d1 m1 a1) > (F d2 m2 a2) = d1>d2 && m1==m2 && a1==a2
                                || m1>m2 && a1==a2
                                || a1>a2
    f1 <= f2 = f1<f2 || f1==f2
    f1 >= f2 = f1>f2 || f1==f2

type Titular = String
type Fuente = String
type Cuerpo = [String]

data Noticia = N Titular Fecha Fuente Cuerpo deriving Eq

instance Ord Noticia where
    (N t1 fech1 f1 c1) < (N t2 fech2 f2 c2) = fech1<fech2
                                            || fech1==fech2 && t1<t2
    (N t1 fech1 f1 c1) > (N t2 fech2 f2 c2) = fech1>fech2
                                            || fech1== fech2 && t1>t2
    n1 <= n2 = n1<n2 || n1==n2
    n1 >= n2 = n1>n2 || n1==n2

instance Show Noticia where
    show (N t fech f c) = " " ++ t ++ " \n" ++ f ++ " - " ++ show fech ++ "\n"++ (unlines c) 
    --El primer espacio porq sino no muestra la primera letra del titulo ¿?
    --unlines de c para que el formato quede bien

type Noticias = [Noticia]

crearFecha:: Int->Int->Int->Fecha
crearFecha d m a = F d m a

crearNoticia::String->Fecha->String->[String]->Noticia
crearNoticia t fe f c = N t fe f c

getTitularNoti :: Noticia -> String
getTitularNoti (N t fech f c) = t

getFechaNoti :: Noticia -> Fecha
getFechaNoti (N t fech f c) = fech

getFuenteNoti :: Noticia -> String
getFuenteNoti (N t fech f c) = f

getCuerpoNoti :: Noticia -> [String]
getCuerpoNoti (N t fech f c) = c