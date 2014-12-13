{-# LANGUAGE ScopedTypeVariables #-}

module Constraints (
        Net(..), Constraint, Node,
        Domain, NodeName,
        mkConstraint, var,
        ac3, solve
    ) where

import Control.Monad.State.Strict
import Data.List (find)
import Data.Maybe (fromJust)

-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domainmenge
data Node a = Node String (Domain a) deriving (Show, Eq)
type Domain a = [a] -- Die Domain ist vom Typ a
type NodeName = String

var :: String -> Domain a -> Node a
var = Node

nodeName :: Node a -> String
nodeName (Node s _) = s

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

mkConstraint :: forall a. Eq a => NodeName -> (a -> a -> Bool) -> NodeName -> String -> Constraint a
mkConstraint s1 f s2 name = Binary name s1 s2 g f
    where
        g :: Node a -> Node a -> (Node a, Bool)
        g xNode@(Node x' xs) (Node y' ys)
                | x' == s1 && y' == s2 =
                    let xs' = [ x | x <- xs, any (f x) ys] -- dies hier ist eine Implementation des REVISE Algorithmus
                    in ( Node x' xs', xs' /= xs )
                | otherwise = (xNode, False)


-- solve takes a net and returns a list of mappings
solve :: forall a. (Eq a, Show a) => Net a -> [(NodeName, a)]
solve = undefined

-- Implementation von ac3. Kantenkonsistenz mit Full Lookahead
-- ac3
ac3 :: forall a. (Eq a, Show a) => Net a -> IO (Net a)
ac3 net@(Net _ cs) = resultNet
    where        
        queue :: [Constraint a]
        queue = concatMap (\c -> [c, flop c]) cs    -- create initial queue of constraints.
        resultNet = evalStateT reviser (net, queue)       -- evalState führt die Stateful Computation aus und gibt den Rückgabewert zurück
        reviser :: Show a => StateT (S a) IO (Net a)
        reviser = do
            (net, cs) <- get
            lift $ putStr "Iteration: "
            lift $ print net
            changes <- replicateM (length cs) reduceSingleConstraint
            (newnet, _) <- get
            if True `elem` changes
                then modify (\(n,_) -> (n,cs)) >> reviser
                else return newnet


-- St a is the type used in the stateful computation.
type S a = (Net a, [Constraint a])
    
reduceSingleConstraint :: Show a => StateT (S a) IO Bool
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
                        when changed
                            (lift (putStrLn $ s1 ++ " vs " ++ s2 ++ ": " ++ show n1 ++ " -> " ++ show n1' ++ ", rn: " ++ show relevant_neighbours))
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

