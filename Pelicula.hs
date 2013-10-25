module Pelicula (Pelicula, nuevaP, nombreP, generosP, actoresP, es3DP, agruparPelisPorGeneroP, generarSagaDePeliculasP) where

import Tipos

data Pelicula = P Nombre [Genero] [Actor] Bool deriving (Show, Eq)

nuevaP :: Nombre -> [Genero] -> [Actor] -> Bool -> Pelicula
nuevaP n g a b = (P n g a b)

nombreP :: Pelicula -> Nombre
nombreP (P n _ _ _) = n

generosP :: Pelicula -> [Genero]
generosP (P _ g _ _) = g

actoresP :: Pelicula -> [Actor]
actoresP (P _ _ a _) = a

es3DP :: Pelicula -> Bool
es3DP (P _ _ _ b) = b

-- Estaba de mas el 'agruparPelisPorGeneroP [] = []' --

agruparPelisPorGeneroP :: [Pelicula] -> [(Genero, [Pelicula])]
agruparPelisPorGeneroP ps = perteneceLista Aventura (pelisDelGeneroP ps Aventura)++perteneceLista Comedia (pelisDelGeneroP ps Comedia)++perteneceLista Drama (pelisDelGeneroP ps Drama)++perteneceLista Romantica (pelisDelGeneroP ps Romantica)++perteneceLista Terror (pelisDelGeneroP ps Terror)

pelisDelGeneroP :: [Pelicula] -> Genero -> [Pelicula]
pelisDelGeneroP [] _ 	 = []
pelisDelGeneroP (p:ps) g | elem g (generosP p)  = p:pelisDelGeneroP ps g
			 | otherwise		= pelisDelGeneroP ps g

perteneceLista :: Genero -> [Pelicula] -> [(Genero, [Pelicula])]
perteneceLista g [] 	= []
perteneceLista g ps = [(g, ps)]

-- Esta funcion no se usa en ningun lado

--dameGenerosP :: [Pelicula] -> [Genero]
--dameGenerosP [] 	= []
--dameGenerosP (p:ps)	= sacarRepetidos(generosP p++dameGenerosP ps)

sacarRepetidosG :: [Genero] -> [Genero]
sacarRepetidosG [] = []
sacarRepetidosG (a:as) 	| elem a as = sacarRepetidosG as
			| otherwise = a:sacarRepetidosG as	

sacarRepetidosA :: [Actor] -> [Actor]
sacarRepetidosA [] = []
sacarRepetidosA (a:as) 	| elem a as = sacarRepetidosA as
			| otherwise = a:sacarRepetidosA as	

-- No sacabamos los repetidos de los generos y actores --

generarSagaDePeliculasP :: [Actor] -> [Genero] -> [Nombre] -> [Pelicula]
generarSagaDePeliculasP _ _ [] = []
generarSagaDePeliculasP as gs (n:ns) = nuevaP n (sacarRepetidosG gs) (sacarRepetidosA as) False:generarSagaDePeliculasP as gs ns

p1 = (P "Star Wars" [Aventura, Comedia] ["Harrison Ford", "Keanu Reeves"] False)
p2 = (P "Pepito" [Aventura, Drama] ["Dustin Hoffman", "Keanu Reeves"] True)
p3 = (P "Indiana Jones" [Comedia, Terror] ["Alberto Olmedo", "Jorge Porcel"] False)

as1 = ["Harrison Ford", "Keanu Reeves", "Dustin Hoffman", "Keanu Reeves"]
gs1 = [Aventura, Comedia, Drama]
ns1 = ["Nombre1", "Nombre2", "Nombre3"]
