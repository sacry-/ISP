{-# LANGUAGE ScopedTypeVariables #-}

module Constraints (
        Net(..), Constraint, Node,
        Domain, NodeName,
        mkConstraint,
        ac1
    ) where

import Control.Monad.State.Strict
import Control.Monad (replicateM)
import Data.List (find)
import Data.Maybe (fromJust)
import Debug.Trace

-- Ein Knoten besteht aus seinem Namen sowie aus dessen Domainmenge
type Node a = (String, Domain a)
type Domain a = [a] -- Die Domain ist vom Typ a
type NodeName = String

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

mkConstraint :: forall a. Eq a => (a -> a -> Bool) -> String-> NodeName -> NodeName -> Constraint a
mkConstraint f name s1 s2 = Binary name s1 s2 g f
    where
        g :: Node a -> Node a -> (Node a, Bool)
        g xNode@(x',xs) (y',ys)
                | x' == s1 && y' == s2 =
                    let xs' = [ x | x <- xs, any (\y -> f x y) ys] -- dies hier ist eine Implementation des REVISE Algorithmus
                    in ( (x', xs'), xs' /= xs )
                | otherwise = (xNode, False)

ac1 :: forall a. (Eq a, Show a) => Net a -> IO (Net a)
ac1 net@(Net ns cs) = result
    where
        result = ns'
        
        queue :: [Constraint a]
        queue = concatMap (\c -> [c, flop c]) cs    -- create initial queue of constraints.
        
        ns' = evalStateT reviser (net, queue)
        
        reviser :: Show a => StateT (S a) IO (Net a)
        reviser = do
            (net, cs) <- get
            lift $ putStr "Iteration: "
            lift $ print net
            changes <- replicateM (length cs) checkSingleConstraint
            (newnet, _) <- get
            if any (==True) changes
                then modify (\(n,_) -> (n,cs)) >> reviser
                else return newnet

-- St a is the type used in the stateful computation.
type S a = (Net a, [Constraint a])
    
checkSingleConstraint :: Show a => StateT (S a) IO Bool
checkSingleConstraint = 
    do
        ((Net ns net_cs), cs) <- get
        case cs of
            [] -> return False
            (c:cs') ->
                let (Binary name s1 s2 f _) = c
                    n1 = fromJust $ find (\n -> fst n == s1) ns
                    n2 = fromJust $ find (\n -> fst n == s2) ns
                    (n1', changed) = f n1 n2
                    ns' = replaceNode n1 n1' ns
                    newnet = Net ns' net_cs
                in
                    do
                        when changed
                            (lift (putStrLn $ s1 ++ " vs " ++ s2 ++ ": " ++ show n1 ++ " -> " ++ show n1'))
                        put (newnet, cs')
                        return changed

-- replaces first occurrence of x in the list by y
replaceNode :: Node a -> Node a -> [Node a] -> [Node a]
replaceNode x y [] = error "Not found in replace" 
replaceNode x y (b:bs)
    | fst b == fst x = y:bs
    | otherwise = b : replaceNode x y bs

-- flop takes a Constraint and builds its flipped Constraint.
flop :: (Show a, Eq a) => Constraint a -> Constraint a
flop (Binary name s1 s2 c f) = mkConstraint (flip f) name s2 s1 

