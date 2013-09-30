module Tipos where

type Sala = Int
type Nombre = String
type Actor = String

data Genero = Aventura | Comedia | Drama | Romantica | Terror deriving (Show, Eq)