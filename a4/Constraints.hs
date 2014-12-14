{-# LANGUAGE ScopedTypeVariables #-}

module Constraints (
        Net(..), Constraint, Node,
        Domain, NodeName,
        mkConstraint, var,
        ac3, solve
        
        
        -- for testing
        ,expandFirstNode
    ) where

import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.List (find)
import Data.Maybe (fromJust)
import Debug.Trace

-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domainmenge
data Node a = Node String (Domain a) deriving (Show, Eq)
type Domain a = [a] -- Die Domain ist vom Typ a
type NodeName = String

var :: String -> Domain a -> Node a
var = Node

nodeName :: Node a -> String
nodeName (Node s _) = s

domain :: Node a -> Domain a
domain (Node _ xs) = xs

-- Net a ist ein Constraint Netz dessen Werte Elemente vom Typ a haben können.
data Net a = Net [Node a] [Constraint a] deriving Show
-- Ein Constraint ist eine unäre/binäry Funktion die ein Node nimmt
-- und ein Node zurückliefert, bei dem fehlerhafte Elemente aus der Domain entfernt wurden.
-- Bei binären Constraints werden für Elemente der ersten Menge Partner in der Zweiten gesucht.
-- Es wird eine Modifikation des ersten Knoten zurückgeliefert sowie ein Bool das eine Änderung makiert.
-- Die Funktionen liefern beim Aufruf auf irrelevante Knoten den gleichen Knoten zurück.
data Constraint a =
    Binary
        String -- name des Constraints
        NodeName -- name des zweiten Knotens
        NodeName -- name des ersten Knotens
        (Node a -> Node a -> (Node a, Bool)) -- Funktion
        (a -> a -> Bool) -- Kopie der originalen Funktion
    
instance Show (Constraint a) where
    show (Binary s _ _ _ _) = "Binary " ++ s


cName1 :: Constraint a -> String
cName1 (Binary _ s _ _ _) = s

cName2 :: Constraint a -> String
cName2 (Binary _ _ s _ _) = s

mkConstraint :: forall a. (Eq a, Show a) => NodeName -> (a -> a -> Bool) -> NodeName -> String -> Constraint a
mkConstraint s1 f s2 name = Binary name s1 s2 g f
    where
        g :: Node a -> Node a -> (Node a, Bool)
        g xNode@(Node x' xs) (Node y' ys)
                | x' == s1 && y' == s2 =
                    let xs' = [ x | x <- xs, any (f x) ys] -- dies hier ist eine Implementation des REVISE Algorithmus
                    in ( Node x' xs', xs' /= xs )
                | otherwise = (xNode, False)


type Solution a = [(NodeName, a)]

-- solve takes a net and returns a list of solutions
-- uses ac3 with Full Lookahead
solve :: forall a. (Eq a, Show a) => Net a -> [Solution a]
solve net_ = case inspect net of
                OK solution -> [solution]
                Empty       -> []
                Expandable  -> expandedSolutions
    where
        net = ac3 net_
        nets = map ac3 $ expandFirstNode net
        expandedSolutions = concatMap solve nets

data Inspection a =
    OK (Solution a)
    | Empty
    | Expandable

-- getSolution returns a solution to the net,
-- if the net already has single elements as its domain
inspect :: Net a -> Inspection a
inspect (Net ns _)
    | any (null . domain) ns                 = Empty
    | any (not . null . drop 1 . domain) ns  = Expandable
    | all (null . drop 1 . domain) ns        = OK $ map (\(Node s [x]) -> (s,x)) ns
    | otherwise = error "Net neither empty, expandable nor has a solution."

-- returns a list of nets where the first node with more than element in domain has been reduced to a singlenton element domain
-- returns empty if there aren't any expansions
expandFirstNode :: forall a. Net a -> [Net a]
expandFirstNode curr@(Net ns_ cs) =
    case ns_ of 
        [] -> []    -- falls ich keine Nodes habe, dann gibts keine relevanten Netze
        ((Node s xs@(_:_:_)):ns) ->     -- falls die Domain des ersten Elements >= 2 ist:
            let newNodess :: [ [Node a] ]
                newNodess = (\x -> (Node s [x]):ns ) `map` xs   -- erzeugen der Liste an [Node] wobei jede NodeList ein anderes Element x ausgewählt hat
                nets = map (\nss -> Net nss cs) newNodess       -- aus jedem NodeList ´wird ein netz erzeugt.
            in  nets
        (n:ns) ->   -- falls der aktuelle Knoten nicht mehr als zwei Elemente hat, wird rekursiv ein nachfolgender Knoten reduziert.
            (\(Net ns' cs) -> Net (n:ns') cs) `map` expandFirstNode (Net ns cs)
--

-- Implementation von ac3. Kantenkonsistenz
ac3 :: forall a. (Eq a, Show a) => Net a -> Net a
ac3 net@(Net _ cs) = resultNet
    where        
        queue :: [Constraint a]
        queue = concatMap (\c -> [c, flop c]) cs    -- create initial queue of constraints.
        resultNet = runIdentity $ evalStateT reviser (net, queue)       -- evalState führt die Stateful Computation aus und gibt den Rückgabewert zurück
        reviser :: Show a => StateT (S a) Identity (Net a)
        reviser = do
            (net, cs) <- get
            if null cs
                then return net
                else reduceSingleConstraint >> reviser


type S a = (Net a, [Constraint a])
    
reduceSingleConstraint :: Show a => StateT (S a) Identity Bool
reduceSingleConstraint = 
    do
        (Net ns ncs, cs) <- get
        case cs of
            [] -> return False
            (c:cr) ->
                let (Binary name s1 s2 f _) = c
                    n1 = fromJust $ find (\n -> nodeName n == s1) ns
                    n2 = fromJust $ find (\n -> nodeName n == s2) ns
                    (n1', changed) = f n1 n2
                    ns' = replaceNode n1 n1' ns
                    newnet = Net ns' ncs
                    inc = incoming (nodeName n1) ncs 
                    relevant_neighbours = filter (\c -> cName1 c /= nodeName n2) inc
                    cs' = relevant_neighbours ++ cr
                in
                    do
                        put (newnet, cs')
                        return changed

incoming :: NodeName -> [Constraint a] -> [Constraint a]
incoming n = filter (\c -> cName2 c == n)

-- replaces first occurrence of x in the list by y
replaceNode :: Node a -> Node a -> [Node a] -> [Node a]
replaceNode x y [] = error "Not found in replace" 
replaceNode x y (b:bs)
    | nodeName b == nodeName x = y:bs
    | otherwise = b : replaceNode x y bs

-- flop takes a Constraint and builds its flipped Constraint.
flop :: (Show a, Eq a) => Constraint a -> Constraint a
flop (Binary name s1 s2 c f) = mkConstraint s2 (flip f) s1 name

