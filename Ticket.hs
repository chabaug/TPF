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

-- Estaba de mas 'peliculaMenosVistaT [t] = peliculaT t' --

peliculaMenosVistaT :: [Ticket] -> Pelicula
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

-- Esta funcion no se usa en ningun lado
--contarPelis :: [Pelicula] -> Pelicula -> Int
--contarPelis [] _ 	= 0
--contarPelis (p:ps) x	| x == p 	= 1 + contarPelis ps x
--			| otherwise 	= contarPelis ps x

contarPeliEnTicket :: [Ticket] -> Pelicula -> Int
contarPeliEnTicket [] _ = 0
contarPeliEnTicket (t:ts) p 	| peliculaT t == p 	= 1 + contarPeliEnTicket ts p
				| otherwise 		= contarPeliEnTicket ts p

sacarRepetidos :: [Pelicula] -> [Pelicula]
sacarRepetidos [] = []
sacarRepetidos (a:as) 	| elem a as = sacarRepetidos as
			| otherwise = a:sacarRepetidos as

todosLosTicketsParaLaMismaSalaT :: [Ticket] -> Bool
todosLosTicketsParaLaMismaSalaT []  	= True
todosLosTicketsParaLaMismaSalaT [t] 	= True
todosLosTicketsParaLaMismaSalaT (t:ts) 	| salaT t == salaT (head ts) 	= todosLosTicketsParaLaMismaSalaT ts
					| otherwise 			= False

{- todosLosTicketsParaLaMismaSalaT :: [Ticket] -> Bool
todosLosTicketsParaLaMismaSalaT ts	| length (ticketsDeLaMismaSala ts) == length ts	= True
					| otherwise					= False

ticketsDeLaMismaSala :: [Ticket] -> [Ticket]
ticketsDeLaMismaSala [] 	= []
ticketsDeLaMismaSala [t]	= [t]
ticketsDeLaMismaSala (t1:t2:ts)	| salaT t1 == salaT t2	= t1:ticketsDeLaMismaSala (t2:ts)
				| otherwise		= ticketsDeLaMismaSala (t2:ts)
-}

cambiarSalaT :: [Ticket] -> Sala -> Sala -> [Ticket]
cambiarSalaT [] _ _ = []
cambiarSalaT ((TicketSinUsar s p):ts) v n 
		| s == v 	= (TicketSinUsar n p):cambiarSalaT ts v n
		| otherwise	= (TicketSinUsar s p):cambiarSalaT ts v n
cambiarSalaT ((TicketUsado (TicketSinUsar s p)):ts) v n  
		| s == v  	= (TicketUsado (TicketSinUsar n p)):cambiarSalaT ts v n
		| otherwise	= (TicketUsado (TicketSinUsar s p)):cambiarSalaT ts v n

-- Objetos para Pruebas
p1 = nuevaP "Pelicula1" [Aventura, Comedia] ["Harrison Ford", "Keanu Reeves"] False
p2 = nuevaP "Pelicula2" [Aventura, Drama] ["Dustin Hoffman", "Keanu Reeves"] True
p3 = nuevaP "Pelicula3" [Comedia, Terror] ["Alberto Olmedo", "Jorge Porcel"] False
p4 = nuevaP "Pelicula4" [Aventura, Comedia] ["Harrison Ford", "Keanu Reeves"] False
p5 = nuevaP "Pelicula5" [Aventura, Drama] ["Dustin Hoffman", "Keanu Reeves"] True
p6 = nuevaP "Pelicula6" [Comedia, Terror] ["Alberto Olmedo", "Jorge Porcel"] False

t1 = nuevoT 2 p1 True
t2 = nuevoT 5 p1 True
t3 = nuevoT 2 p2 True
t4 = nuevoT 2 p2 True
t5 = nuevoT 2 p2 True
t6 = nuevoT 2 p2 True




