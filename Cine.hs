module Cine (nuevoC, nombreC, peliculasC, salasC, espectadoresC, salaC, ticketsVendidosC,  abrirSalaC, agregarPeliculaC, cerrarSalaC, cerrarSalasC, cerrarSalasDeLaCadenaC, peliculaC, venderTicketC, ingresarASalaC, pasarA3DUnaPeliculaC ) where

import Tipos
import Pelicula
import Ticket

data Cine = C Nombre | 
			SalaSinPelicula Cine Sala | 
			SalaConPelicula Cine Sala Pelicula Int | 
			TicketVendido Cine Ticket deriving (Show)

nuevoC :: Nombre -> Cine
nuevoC n = (C n)

nombreC :: Cine -> Nombre
nombreC (C n) 				= n
nombreC (SalaSinPelicula (C n) _) 	= n
nombreC (SalaConPelicula (C n) _ _ _) 	= n
nombreC (TicketVendido (C n) _)		= n

peliculasC :: Cine -> [Pelicula]
peliculasC (C _) 			= []
peliculasC (SalaSinPelicula c _) 	= peliculasC c
peliculasC (SalaConPelicula c _ p _) 	= p:peliculasC c
peliculasC (TicketVendido c _) 		= peliculasC c

salasC :: Cine -> [Sala]
salasC (C _) 				= []
salasC (SalaSinPelicula c s) 		= s:salasC c
salasC (SalaConPelicula c s _ _) 	= s:salasC c
salasC (TicketVendido c _)		= salasC c

espectadoresC :: Cine -> Sala -> Int
espectadoresC (C _) _ = 0
espectadoresC (SalaSinPelicula c s) x 
		| s == x 	= 0
		| otherwise 	= espectadoresC c x
espectadoresC (SalaConPelicula c s p i) x
		| s == x 	= i
		| otherwise	= espectadoresC c x
espectadoresC (TicketVendido c _) x = espectadoresC c x

salaC :: Cine -> Pelicula -> Sala
salaC (SalaSinPelicula c s) x 	= salaC c x
salaC (SalaConPelicula c s p i) x	| p == x 	= s
					| otherwise 	= salaC c x
salaC (TicketVendido c _) x	= salaC c x

ticketsVendidosC :: Cine -> [Ticket]
ticketsVendidosC (C _) 				= []
ticketsVendidosC (SalaSinPelicula c _) 		= ticketsVendidosC c
ticketsVendidosC (SalaConPelicula c _ _ _) 	= ticketsVendidosC c
ticketsVendidosC (TicketVendido c t) 		= t:ticketsVendidosC c

abrirSalaC :: Cine -> Sala -> Cine
abrirSalaC c s = (SalaSinPelicula c s)

agregarPeliculaC :: Cine -> Pelicula -> Sala -> Cine
agregarPeliculaC (SalaSinPelicula c s) p x 
			| x == s 		= (SalaConPelicula c x p 0)
			| otherwise		= (SalaSinPelicula (agregarPeliculaC c p x) s)
agregarPeliculaC (SalaConPelicula c s j i) p x 	= (SalaConPelicula (agregarPeliculaC c p x) s j i)
agregarPeliculaC (TicketVendido c t) p x	= (TicketVendido (agregarPeliculaC c p x) t)

cerrarSalaC :: Cine -> Sala -> Cine
cerrarSalaC (SalaSinPelicula c s) x 	| s == x 	= c
					| otherwise 	= (SalaSinPelicula (cerrarSalaC c x) s)
cerrarSalaC (SalaConPelicula c s p i) x | s == x 	= c
					| otherwise	= (SalaConPelicula (cerrarSalaC c x) s p i)
cerrarSalaC (TicketVendido c t) x			= (TicketVendido (cerrarSalaC c x) t)

cerrarSalasC :: Cine -> Int -> Cine
cerrarSalasC (C n) x			= (C n)
cerrarSalasC (SalaSinPelicula c s) x	= (SalaSinPelicula (cerrarSalasC c x) s)
cerrarSalasC (SalaConPelicula c s p i) x 
			| x > i 	= cerrarSalasC c x
			| otherwise 	= (SalaConPelicula (cerrarSalasC c x) s p i)
cerrarSalasC (TicketVendido c t) x	= (TicketVendido (cerrarSalasC c x) t)

cerrarSalasDeLaCadenaC :: [Cine] -> Int -> [Cine]
cerrarSalasDeLaCadenaC [] _ 	= []
cerrarSalasDeLaCadenaC (c:cs) x = cerrarSalasC c x:cerrarSalasDeLaCadenaC cs x

peliculaC :: Cine -> Sala -> Pelicula
peliculaC (SalaSinPelicula c s) x 	= peliculaC c x
peliculaC (SalaConPelicula c s p i) x 	| x == s    = p
					| otherwise = peliculaC c x
peliculaC (TicketVendido c t) x		= peliculaC c x

venderTicketC :: Cine -> Pelicula -> (Cine, Ticket)
venderTicketC (SalaSinPelicula c s) x = ((SalaSinPelicula (venderTicketAux c x) s), (nuevoT (salaC (SalaSinPelicula c s) x) x False))
venderTicketC (SalaConPelicula c s p i) x | x == p 	= ((TicketVendido (SalaConPelicula c s p i) (nuevoT s p False)), (nuevoT s p False)) | otherwise	= ((SalaConPelicula (venderTicketAux c x) s p i), (nuevoT (salaC (SalaConPelicula c s p i) x) x False))
venderTicketC (TicketVendido c t) x
		= ((TicketVendido (venderTicketAux c x) t), (nuevoT (salaC (TicketVendido c t) x) x False))

venderTicketAux :: Cine -> Pelicula -> Cine
venderTicketAux	(SalaSinPelicula c s) x		= (SalaSinPelicula (venderTicketAux c x) s)
venderTicketAux (SalaConPelicula c s p i) x	
		| x == p 	= (TicketVendido (SalaConPelicula c s p i) (nuevoT s p False))
		| otherwise	= (SalaConPelicula (venderTicketAux c x) s p i)
venderTicketAux (TicketVendido c t) x		= (TicketVendido (venderTicketAux c x) t)

ingresarASalaC :: Cine -> Sala -> Ticket -> (Cine, Ticket)
ingresarASalaC (SalaSinPelicula c s) x y 	= ((SalaSinPelicula (ingresarASalaAux c x y) s), (usarT y))
ingresarASalaC (SalaConPelicula c s p i) x y
		| s == x	= ((SalaConPelicula (ingresarASalaAux c x y) s p (i+1)), (usarT y))
		| otherwise	= ((SalaConPelicula (ingresarASalaAux c x y) s p i), (usarT y))
ingresarASalaC (TicketVendido c t) x y
		| t == y		= ((ingresarASalaAux c x y), (usarT y))
		| otherwise		= ((TicketVendido (ingresarASalaAux c x y) t), (usarT y))

ingresarASalaAux :: Cine -> Sala -> Ticket -> Cine
ingresarASalaAux (C n) _ _ = (C n)
ingresarASalaAux (SalaSinPelicula c s) x y = (SalaSinPelicula (ingresarASalaAux c x y) s)
ingresarASalaAux (SalaConPelicula c s p i) x y
		| s == x 	= (SalaConPelicula (ingresarASalaAux c x y) s p (i+1))
		| otherwise	= (SalaConPelicula (ingresarASalaAux c x y) s p i)
ingresarASalaAux (TicketVendido c t) x y 
		| t == y		= ingresarASalaAux c x y
		| otherwise		= (TicketVendido (ingresarASalaAux c x y) t)

pasarA3DUnaPeliculaC :: Cine -> Nombre -> (Cine, Pelicula)
pasarA3DUnaPeliculaC (SalaSinPelicula c s) n
	= ((SalaSinPelicula (modificaPeliDelCineAux c n) s), (hacer3DUnaPeliAux c n))
pasarA3DUnaPeliculaC (SalaConPelicula c s p i) n
	| n == nombreP p
	= ((SalaConPelicula c s (nuevaP n (generosP p) (actoresP p) True) i), (nuevaP n (generosP p) (actoresP p) True))
	| otherwise	
	= ((SalaConPelicula (modificaPeliDelCineAux c n) s p i), (hacer3DUnaPeliAux c n))
pasarA3DUnaPeliculaC (TicketVendido c t) n 
	= ((TicketVendido (modificaPeliDelCineAux c n) t), (hacer3DUnaPeliAux c n))

modificaPeliDelCineAux :: Cine -> Nombre -> Cine
modificaPeliDelCineAux (C n) _ = (C n)
modificaPeliDelCineAux (SalaSinPelicula c s) n = (SalaSinPelicula (modificaPeliDelCineAux c n) s)
modificaPeliDelCineAux (SalaConPelicula c s p i) n
		| n == nombreP p	= (SalaConPelicula c s (nuevaP n (generosP p) (actoresP p) True) i)
		| otherwise		= (SalaConPelicula (modificaPeliDelCineAux c n) s p i)
modificaPeliDelCineAux (TicketVendido c t) n = (TicketVendido (modificaPeliDelCineAux c n) t)

hacer3DUnaPeliAux :: Cine -> Nombre -> Pelicula
hacer3DUnaPeliAux (SalaSinPelicula c s) n 	= hacer3DUnaPeliAux c n
hacer3DUnaPeliAux (SalaConPelicula c s p i) n 
			| n == nombreP p	= nuevaP n (generosP p) (actoresP p) True
			| otherwise		= hacer3DUnaPeliAux c n
hacer3DUnaPeliAux (TicketVendido c t) n 	= hacer3DUnaPeliAux c n




