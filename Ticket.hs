module Ticket (Ticket, nuevoT, salaT, peliculaT, usadoT, usarT, peliculaMenosVistaT, todosLosTicketsParaLaMismaSalaT, cambiarSalaT) 	where

import Tipos
import Pelicula

data Ticket = TicketSinUsar Sala Pelicula | TicketUsado Ticket deriving (Show, Eq)

nuevoT :: Sala -> Pelicula -> Bool -> Ticket
nuevoT s p b 	| b == False 	= (TicketSinUsar s p)
		| b == True	= (TicketUsado (TicketSinUsar s p))

salaT :: Ticket -> Sala
salaT (TicketSinUsar s _) 		= s
salaT (TicketUsado (TicketSinUsar s _)) = s

peliculaT :: Ticket -> Pelicula
peliculaT (TicketSinUsar _ p) 			= p
peliculaT (TicketUsado (TicketSinUsar _ p)) 	= p

usadoT :: Ticket -> Bool
usadoT (TicketSinUsar _ _) 	= False
usadoT (TicketUsado _) 		= True

usarT :: Ticket -> Ticket
usarT t = (TicketUsado t)

peliculaMenosVistaT :: [Ticket] -> Pelicula
peliculaMenosVistaT [t] = peliculaT t
peliculaMenosVistaT (ts) = menosApariciones (sacarRepetidos(damePeliculasT ts)) (dameUsadosT ts)

menosApariciones :: [Pelicula] -> [Ticket] -> Pelicula
menosApariciones [p] _ = p
menosApariciones (p1:p2:ps) ts 	| contarPeliEnTicket ts p1 <= contarPeliEnTicket ts p2  = menosApariciones (p1:ps) ts
				| otherwise 						= menosApariciones (p2:ps) ts

dameUsadosT :: [Ticket] -> [Ticket]
dameUsadosT [] = []
dameUsadosT (t:ts) | usadoT t  = t:dameUsadosT ts
		   | otherwise = dameUsadosT ts

damePeliculasT :: [Ticket] -> [Pelicula]
damePeliculasT [] = []
damePeliculasT (t:ts) = peliculaT t:damePeliculasT ts

contarPelis :: [Pelicula] -> Pelicula -> Int
contarPelis [] _ 	= 0
contarPelis (p:ps) x	| x == p 	= 1 + contarPelis ps x
			| otherwise 	= contarPelis ps x

contarPeliEnTicket :: [Ticket] -> Pelicula -> Int
contarPeliEnTicket [] _ = 0
contarPeliEnTicket (t:ts) p 	| peliculaT t == p 	= 1 + contarPeliEnTicket ts p
				| otherwise 		= contarPeliEnTicket ts p

sacarRepetidos :: [Pelicula] -> [Pelicula]
sacarRepetidos [] = []
sacarRepetidos (a:as) 	| elem a as = sacarRepetidos as
			| otherwise = a:sacarRepetidos as

todosLosTicketsParaLaMismaSalaT :: [Ticket] -> Bool
todosLosTicketsParaLaMismaSalaT ts	| length (ticketsDeLaMismaSala ts) == length ts	= True
					| otherwise					= False

ticketsDeLaMismaSala :: [Ticket] -> [Ticket]
ticketsDeLaMismaSala [] 	= []
ticketsDeLaMismaSala [t]	= [t]
ticketsDeLaMismaSala (t1:t2:ts)	| salaT t1 == salaT t2	= t1:ticketsDeLaMismaSala (t2:ts)
				| otherwise		= ticketsDeLaMismaSala (t2:ts)

cambiarSalaT :: [Ticket] -> Sala -> Sala -> [Ticket]
cambiarSalaT [] _ _ = []
cambiarSalaT ((TicketSinUsar s p):ts) v n 
		| salaT (TicketSinUsar s p) == v = (TicketSinUsar v p):cambiarSalaT ts v n
		| otherwise			 = (TicketSinUsar s p):cambiarSalaT ts v n
cambiarSalaT ((TicketUsado (TicketSinUsar s p)):ts) v n 
		| salaT (TicketUsado (TicketSinUsar s p)) == v  = (TicketUsado (TicketSinUsar n p)):cambiarSalaT ts v n
		| otherwise			 		= (TicketUsado (TicketSinUsar s p)):cambiarSalaT ts v n






