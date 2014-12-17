{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

import Constraints
import Data.List hiding (all)
import Data.Maybe
import Debug.Trace
import Control.Applicative
import Prelude  hiding (all)

-- main = ac3 `with` fullLookAhead net
main :: IO ()
main = 
    let sols = take 3 $ solve netEinstein
    in  mapM_ printSolution sols


netEinstein, netSimple :: Net Person
netEinstein = {-addTripleConstraints-} netSimple

netSimple = Net
        [
             var "brit" ps,
             var "swed" ps,
             var "dane" ps,
             var "norwegian" ps,
             var "german" ps,

             var "l" ps,
             var "hl" ps,
             var "m" ps,
             var "hr" ps,
             var "r" ps,

             var "red" ps,
             var "white" ps,
             var "blue" ps,
             var "green" ps,
             var "yellow" ps,

             var "tea" ps,
             var "coffee" ps,
             var "milk" ps,
             var "beer" ps,
             var "water" ps,

             var "pallmall" ps,
             var "dunhill" ps,
             var "malboro" ps,
             var "rothmanns" ps,
             var "winfield" ps,

             var "dog" ps,
             var "cat" ps,
             var "bird" ps,
             var "fish" ps,
             var "horse" ps
        ]
        [   
            mkConstraint "brit" (==) "red" "Der Brite lebt im roten Haus.",
            mkConstraint "swed" (==) "dog" "Der Schwede hält sich einen Hund.",
            mkConstraint "dane" (==) "tea" "Der Däne trinkt gern Tee.",
            mkConstraint "green" (==) "coffee" "Der Besitzer des grünen Hauses trinkt Kaffee.",
            mkConstraint "pallmall" (==) "bird" "Die Person, die Pall Mall raucht, hat einen Vogel.",
            mkConstraint "m" (==) "milk" "Der Mann im mittleren Haus trinkt Milch.",
            mkConstraint "yellow" (==) "dunhill" "Der Bewohner des gelben Hauses raucht Dunhill.",
            mkConstraint "norwegian" (==) "l" "Der Norweger lebt im ersten Haus.",
            mkConstraint "winfield" (==) "beer" "Der Winfield-Raucher trinkt gern Bier.",
            mkConstraint "german" (==) "rothmanns" "Der Deutsche raucht Rothmanns."
        ]
{-
addTripleConstraints :: Net MS -> Net MS
addTripleConstraints net0 =
        let net1 = addConstraint3 net0 "cgr" "pos" "ani" c10 t10 "Der Malboro-Raucher wohnt neben der Person mit der Katze."
            net2 = addConstraint3 net1 "cgr" "pos" "drk" c11 t11 "Der Malboro-Raucher hat einen Nachbarn, der Wasser trinkt."
            net3 = addConstraint3 net2 "ani" "pos" "cgr" c13 t13 "Der Mann mit dem Pferd lebt neben der Person, die Dunhill raucht."
            netZ = addConstraint3 net3 "nat" "pos" "col" c15 t15 "Der Norweger wohnt neben dem blauen Haus."
            mkConstraint "green" c4 "white" "Das grüne Haus steht links neben dem weißen Haus."
        in  net1
-}

data Person = A | B | C | D | E | Triple (Person, Person, Person)
    deriving(Show, Eq, Read)

ps :: [Person]
ps = [A, B, C, D, E]

printSolution sols =
    mapM_ (\(n, ls) -> putStrLn $ n ++ " -> " ++ show ls) sols
    >> putStrLn "-------------"

