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

-- No era recursiva. Ahora si --
nombreC :: Cine -> Nombre
nombreC (C n) 				= n
nombreC (SalaSinPelicula c _) 		= nombreC c
nombreC (SalaConPelicula c _ _ _) 	= nombreC c
nombreC (TicketVendido c _)		= nombreC c

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

-- Estaba de mas 'espectadoresC (C _) _ = 0' --
espectadoresC :: Cine -> Sala -> Int
espectadoresC (SalaSinPelicula c s) x 
		| s == x 	= 0
		| otherwise 	= espectadoresC c x
espectadoresC (SalaConPelicula c s p i) x
		| s == x 	= i
		| otherwise	= espectadoresC c x
espectadoresC (TicketVendido c _) x = espectadoresC c x

-- Estaba mal (SalaConPelicula c s p i), habia que poner _ en vez de i --
salaC :: Cine -> Pelicula -> Sala
salaC (SalaSinPelicula c s) x 	= salaC c x
salaC (SalaConPelicula c s p _) x	| p == x 	= s
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

-- Aca la correccion decia: 'Para que se toman el trabajo de tener los tickets despues del SalaConPelicula de la sala?' 
-- Este es el 'venderTicketC' que esta mal, abajo pongo el que hice yo' --
venderTicketC :: Cine -> Pelicula -> (Cine, Ticket)
venderTicketC (SalaSinPelicula c s) x = ((SalaSinPelicula (venderTicketAux c x) s), (nuevoT (salaC (SalaSinPelicula c s) x) x False))
venderTicketC (SalaConPelicula c s p i) x | x == p 	= ((TicketVendido (SalaConPelicula c s p i) (nuevoT s p False)), (nuevoT s p False)) | otherwise	= ((SalaConPelicula (venderTicketAux c x) s p i), (nuevoT (salaC (SalaConPelicula c s p i) x) x False))
venderTicketC (TicketVendido c t) x
		= ((TicketVendido (venderTicketAux c x) t), (nuevoT (salaC (TicketVendido c t) x) x False))
-- ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑BORRAR↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ --
-- Aca hay otra correccion que dice 'Estas funciones que tienen que devolver tuplas las pueden escribir como (modificar cine, crear algo) y asi evitan estar todo el tiempo escribiendo tuplas' --

venderTicketAux :: Cine -> Pelicula -> Cine
venderTicketAux	(SalaSinPelicula c s) x		= (SalaSinPelicula (venderTicketAux c x) s)
venderTicketAux (SalaConPelicula c s p i) x	
		| x == p 	= (TicketVendido (SalaConPelicula c s p i) (nuevoT s p False))
		| otherwise	= (SalaConPelicula (venderTicketAux c x) s p i)
venderTicketAux (TicketVendido c t) x		= (TicketVendido (venderTicketAux c x) t)
-- ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑BORRAR↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ --

-- Aca va el 'venderTicketC' que hice yo. Notaran que se simplifico muchisimo, venderTicketAux es la misma funcion--
venderTicketC :: Cine -> Pelicula -> (Cine, Ticket)
venderTicketC c p = ((venderTicketAux c p), (nuevoT (salaC c p) p False))

venderTicketAux :: Cine -> Pelicula -> Cine
venderTicketAux	(SalaSinPelicula c s) x		= (SalaSinPelicula (venderTicketAux c x) s)
venderTicketAux (SalaConPelicula c s p i) x	
		| x == p 	= (TicketVendido (SalaConPelicula c s p i) (nuevoT s p False))
		| otherwise	= (SalaConPelicula (venderTicketAux c x) s p i)
venderTicketAux (TicketVendido c t) x		= (TicketVendido (venderTicketAux c x) t)

-- La correccion dice: 'Pueden hacer directamente ingresarASalaAux cine y en ingresarASalaC no hacer pattern matching'
-- Tambien nos dice 'Por que sigue llamando?' cuando llamamos ingresarASalaAux cuando s == x, y 'Esto marca mas de un ticket' cuando t == y
 
ingresarASalaC :: Cine -> Sala -> Ticket -> (Cine, Ticket)
ingresarASalaC (SalaSinPelicula c s) x y 	= ((SalaSinPelicula (ingresarASalaAux c x y) s), (usarT y))
ingresarASalaC (SalaConPelicula c s p i) x y
		| s == x	= ((SalaConPelicula (ingresarASalaAux c x y) s p (i+1)), (usarT y))
		| otherwise	= ((SalaConPelicula (ingresarASalaAux c x y) s p i), (usarT y))
ingresarASalaC (TicketVendido c t) x y
		| t == y		= ((ingresarASalaAux c x y), (usarT y))
		| otherwise		= ((TicketVendido (ingresarASalaAux c x y) t), (usarT y))

-- De esta funcion la correccion dice 'Ingresa a todos los tickets'

ingresarASalaAux :: Cine -> Sala -> Ticket -> Cine
ingresarASalaAux (C n) _ _ = (C n)
ingresarASalaAux (SalaSinPelicula c s) x y = (SalaSinPelicula (ingresarASalaAux c x y) s)
ingresarASalaAux (SalaConPelicula c s p i) x y
		| s == x 	= (SalaConPelicula (ingresarASalaAux c x y) s p (i+1))
		| otherwise	= (SalaConPelicula (ingresarASalaAux c x y) s p i)
ingresarASalaAux (TicketVendido c t) x y 
		| t == y		= ingresarASalaAux c x y
		| otherwise		= (TicketVendido (ingresarASalaAux c x y) t)
-- ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑BORRAR↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ --

-- Aca va el 'ingresarASalaC' que hice yo --

ingresarASalaC :: Cine -> Sala -> Ticket -> (Cine, Ticket)
ingresarASalaC c s t = ((usaTicket (entraEspectador c s) t), (usarT t))

entraEspectador :: Cine -> Sala -> Cine
entraEspectador (SalaSinPelicula c s) x = (SalaSinPelicula (entraEspectador c x) s)
entraEspectador (SalaConPelicula c s p i) x | s == x 	= (SalaConPelicula c s p (i+1))
					    | otherwise	= (SalaConPelicula (entraEspectador c x) s p i)
entraEspectador (TicketVendido c t) x = (TicketVendido (entraEspectador c x) t)

usaTicket :: Cine -> Ticket -> Cine
usaTicket (SalaSinPelicula c s) x     = (SalaSinPelicula (usaTicket c x) s)
usaTicket (SalaConPelicula c s p i) x = (SalaConPelicula (usaTicket c x) s p i)
usaTicket (TicketVendido c t) x	| x == t    = (TicketVendido c (usarT t))
				| otherwise = (TicketVendido (usaTicket c x) t)

-- En esta funcion solamente corrigieron 'No pasan los tickets a 3D'. Abajo pongo la que hice yo --

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
-- ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑BORRAR↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ --

-- Aca va 'pasarA3DUnaPeliculaC' que hice yo (habia que sacar la linea de (C n) de modificaPeliDelCineAux tambien) --

pasarA3DUnaPeliculaC :: Cine -> Nombre -> (Cine, Pelicula)
pasarA3DUnaPeliculaC c n = ((pasoTicketsA3D (modificaPeliDelCineAux c n) n), (hacer3DUnaPeliAux c n))

modificaPeliDelCineAux :: Cine -> Nombre -> Cine
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

pasoTicketsA3D :: Cine -> Nombre -> Cine
pasoTicketsA3D (C n) x = (C n)
pasoTicketsA3D (SalaSinPelicula c s) x = (SalaSinPelicula (pasoTicketsA3D c x) s)
pasoTicketsA3D (SalaConPelicula c s p i) x = (SalaConPelicula (pasoTicketsA3D c x) s p i)
pasoTicketsA3D (TicketVendido c t) x	| nombreP (peliculaT t) == x = (TicketVendido (pasoTicketsA3D c x) (hago3D t))
					| otherwise		     = (TicketVendido (pasoTicketsA3D c x) t)

hago3D :: Ticket -> Ticket
hago3D t = nuevoT (salaT t) (nuevaP (nombreP (peliculaT t)) (generosP (peliculaT t)) (actoresP (peliculaT t)) True) True

-- Aca van los problemas nuevos --
-- ingresanMuchosC (Me parece que en la especificacion falta un requiere para que los tickets que entran esten sin usar, pero no especifica. Pongalo en el informe) --

ingresanMuchosC :: Cine -> Sala -> [Ticket] -> (Cine, [Ticket])
ingresanMuchosC c s ts = ((usanLosTickets (entranEspectadores c s ts) ts), (usarTickets ts))

entranEspectadores :: Cine -> Sala -> [Ticket] -> Cine
entranEspectadores (SalaSinPelicula c s) x ys = (SalaSinPelicula (entranEspectadores c x ys) s)
entranEspectadores (SalaConPelicula c s p i) x ys | x == s = (SalaConPelicula c s p (i + length ts))
						  | otherwise = (SalaConPelicula (entranEspectadores c x ys) s p i)
entranEspectadores (TicketVendido c t) x ys   = (TicketVendido (entranEspectadores c x ys) t)

usanLosTickets :: Cine -> [Ticket] -> Cine
usanLosTickets (C n) ts = (C n)
usanLosTickets (SalaSinPelicula c s) ts = (SalaSinPelicula (usanLosTickets c ts) s)
usanLosTickets (SalaConPelicula c s p i) ts = (SalaConPelicula (usanLosTickets c ts) s p i)
usanLosTickets (TicketVendido c t) ts   | elem t ts = (TicketVendido (usanLosTickets c ts) (usarT t))
					| otherwise = (TicketVendido (usanLosTickets c ts) t)

usarTickets :: [Ticket] -> [Ticket]
usarTickets [] = []
usarTickets (t:ts) = usarT t:usarTickets ts

-- agregarGenerosC (Aca no recuerdo como se pone el pattern matching de una lista, puse [_] pero quizas va solo _ )--

agregarGenerosC :: Cine -> [Pelicula] -> Genero -> Cine
agregarGenerosC (C n) [_] _ = (C n)
agregarGenerosC (SalaSinPelicula c s) xs g = agregarGenerosC c xs g
agregarGenerosC (SalaConPelicula c s p i) xs g 
| elem p xs = (SalaConPelicula (agregarGenerosC c xs g) s (nuevaP (nombreP p) (g:generosP p) (actoresP p) (es3DP p)) i)
| otherwise = (SalaConPelicula (agregarGenerosC c xs g) s p i)
agregarGenerosC (TicketVendido c t) xs g = agregarGenerosC c xs g


































