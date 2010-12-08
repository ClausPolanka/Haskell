module Aufgabe8 where

type Vertex = Integer
type Origin = Vertex
type Destination = Vertex

data Color = Red | Blue | Green | Yellow deriving (Eq, Show)
data Ugraph = Ug [(Origin, Color, [Destination])] deriving (Eq, Show)

-- Input:  Ug [(0,Red,[1,2,3]), (1,Red,[0,2,3]), (2,Red,[]), (3,Red,[])]
-- Result: Ug [(0,Red,[1,2,3]),(1,Blue,[0,2,3]),(2,Green,[]),(3,Green,[])])
color :: Ugraph -> Maybe Ugraph
color graph = Nothing