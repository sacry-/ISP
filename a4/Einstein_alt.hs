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


netEinstein, netSimple :: Net MS
netEinstein = addTripleConstraints netSimple

netSimple = Net
        [   
             var "pos" $ mkDom ps (all :: [Position])
            ,var "nat" $ mkDom ps (all :: [Nationality])
            ,var "col" $ mkDom ps (all :: [Color])
            ,var "drk" $ mkDom ps (all :: [Drink])
            ,var "cgr" $ mkDom ps (all :: [Cigarete])
            ,var "ani" $ mkDom ps (all :: [Animal])
        ]
        [   
            mkConstraint "nat" c1 "col" "Der Brite lebt im roten Haus.",
            mkConstraint "nat" c2 "ani" "Der Schwede hält sich einen Hund.",
            mkConstraint "nat" c3 "drk" "Der Däne trinkt gern Tee.",
            mkConstraint "col" c4 "pos" "Das grüne Haus steht links neben dem weißen Haus.",
            mkConstraint "col" c5 "drk" "Der Besitzer des grünen Hauses trinkt Kaffee.",
            mkConstraint "cgr" c6 "ani" "Die Person, die Pall Mall raucht, hat einen Vogel.",
            mkConstraint "pos" c7 "drk" "Der Mann im mittleren Haus trinkt Milch.",
            mkConstraint "col" c8 "cgr" "Der Bewohner des gelben Hauses raucht Dunhill.",
            mkConstraint "nat" c9 "pos" "Der Norweger lebt im ersten Haus.",
            mkConstraint "cgr" c12 "drk" "Der Winfield-Raucher trinkt gern Bier.",
            mkConstraint "nat" c14 "cgr" "Der Deutsche raucht Rothmanns."
        ]

addTripleConstraints :: Net MS -> Net MS
addTripleConstraints net0 =
        let net1 = addConstraint3 net0 "cgr" "pos" "ani" c10 t10 "Der Malboro-Raucher wohnt neben der Person mit der Katze."
            net2 = addConstraint3 net1 "cgr" "pos" "drk" c11 t11 "Der Malboro-Raucher hat einen Nachbarn, der Wasser trinkt."
            net3 = addConstraint3 net2 "ani" "pos" "cgr" c13 t13 "Der Mann mit dem Pferd lebt neben der Person, die Dunhill raucht."
            netZ = addConstraint3 net3 "nat" "pos" "col" c15 t15 "Der Norweger wohnt neben dem blauen Haus."
        in  net1

-- net "cgr" "pos" "ani" c10 "Der Malboro-Raucher wohnt neben der Person mit der Katze."

type MS = Mapping String

c1, c2, c3, c4, c5, c6, c7, c8, c9, c12, c14 :: MS -> MS -> Bool

c1 nm cm = personBy Brit nm == personBy Red cm
c2 nm cm = personBy Swed nm == personBy Dog cm
c3 nm cm = personBy Dane nm == personBy Tea cm

c4 cm ps = p1Pos /= (maxBound :: Position) && succ p1Pos == p2Pos
    where
        p1 = personBy Green cm
        p2 = personBy White cm
        p1Pos = propertyBy p1 ps
        p2Pos = propertyBy p2 ps

c5 nm cm = personBy Green nm == personBy Coffee cm
c6 nm cm = personBy Pallmall nm == personBy Bird cm
c7 nm cm = personBy M nm == personBy Milk cm
c8 nm cm = personBy Yellow nm == personBy Dunhill cm

c9 nm cm = personBy Norwegian nm == personBy L cm
c12 nm cm = personBy Winfield nm == personBy Beer cm
c14 nm cm = personBy German nm == personBy Rothmanns cm

c10, c11, c13, c15 :: MS -> MS -> MS -> Bool

-- Der Malboro-Raucher wohnt neben der Person mit der Katze.
t10 :: (Cigarete, Position, Animal)
c10 cm pm am = neighbours Malboro cm Cat am pm

-- Der Malboro-Raucher hat einen Nachbarn, der Wasser trinkt.
t11 :: (Cigarete, Position, Drink)
c11 cm pm dm = neighbours Malboro cm Water dm pm

-- "Der Mann mit dem Pferd lebt neben der Person, die Dunhill raucht."
t13 :: (Animal, Position, Cigarete)
c13 am pm cm = neighbours Horse am Dunhill cm pm

-- "Der Norweger wohnt neben dem blauen Haus."
t15 :: (Nationality, Position, Color)
c15 nm pm cm = neighbours Norwegian nm Blue cm pm

neighbours :: (Constrainable a, Constrainable b) => a -> MS -> b -> MS -> MS -> Bool
neighbours a am b bm pm = absDiff (fromEnum p1Pos) (fromEnum p2Pos) == 1
    where
        p1 = personBy a am
        p2 = personBy b bm
        p1Pos, p2Pos :: Position
        p1Pos = propertyBy p1 pm
        p2Pos = propertyBy p2 pm
        absDiff a b = abs(a - b)

data Person = A | B | C | D | E 
    deriving(Show, Eq, Bounded, Enum, Read)

data Position = L | HL | M | HR | R 
    deriving(Show, Eq, Bounded, Enum, Read)

data Nationality = Brit | Swed | Dane | Norwegian | German 
    deriving(Show, Eq, Bounded, Enum, Read)

data Color = Red | Green | White | Blue | Yellow 
    deriving(Show, Eq, Bounded, Enum, Read)

data Drink = Tea | Coffee | Milk | Beer | Water 
    deriving(Show, Eq, Bounded, Enum, Read)

data Cigarete = Pallmall | Dunhill | Malboro | Winfield | Rothmanns 
    deriving(Show, Eq, Bounded, Enum, Read)

data Animal = Dog | Cat | Bird | Horse | Fish 
    deriving(Show, Eq, Bounded, Enum, Read)

type Mapping a = [(Person, a)]
type Constrainable a = (Show a, Eq a, Bounded a, Enum a, Read a)

ps :: [Person]
ps = [A .. E]

mkDummydom :: Constrainable a => [a] -> Domain MS
mkDummydom = map (\p -> [(undefined, show p)])

mkDom :: Show a => [Person] -> [a] -> Domain MS
mkDom ps elems = map (\perm -> to5Tuple ps perm) (permutations elems) 

all :: (Bounded a, Enum a) => [a]
all = [minBound .. maxBound]

to5Tuple :: Show a => [Person] -> [a] -> MS
to5Tuple xs@(_:_:_:_:_:[]) ys@(_:_:_:_:_:[]) =
    zipWith (\x y -> (x, show y)) xs ys
to5Tuple _ _ = error "Sorry, elems have to be of length 5 :("

personBy :: Show a => a -> MS -> Person
personBy a = fst . fromJust . find (\(p,nat) -> (nat == show a))

propertyBy :: Read a => Person -> MS -> a
propertyBy p = read . snd . fromJust . find (\(p', pos) -> p' == p)

printSolution sols =
    mapM_ (\(n, ls) -> putStrLn $ n ++ " -> " ++ show ls) sols
    >> putStrLn "-------------"


addConstraint3 :: forall a b c.
    (Constrainable a, Constrainable b, Constrainable c) =>
    Net MS
    -> NodeName -> NodeName -> NodeName
    -> (MS -> MS -> MS -> Bool)
    -> (a,b,c)
    -> String
    -> Net MS
addConstraint3 (Net ns cs) s1 s2 s3 f _ cname = Net ns' cs'
    where
        ns' :: [Node MS]
        ns' = hiddenU:ns

        hiddenU :: Node MS
        hiddenU = var cname (map msTpl2ms uDom)
        
        uDom :: Domain (MS,MS,MS)
        uDom = filter (\(a,b,c) -> f a b c) $ (,,) <$> d1 <*> d2 <*> d3
        
        d1 :: Domain MS
        (Node _ d1) = findNode s1 ns
        (Node _ d2) = findNode s2 ns
        (Node _ d3) = findNode s3 ns

        cs' = cs ++ [
                mkConstraint cname (compareBy (\(a,_,_) -> a)) s1 (s1 ++ " in " ++ cname),
                mkConstraint cname (compareBy (\(_,b,_) -> b)) s2 (s2 ++ " in " ++ cname),
                mkConstraint cname (compareBy (\(_,_,c) -> c)) s3 (s3 ++ " in " ++ cname)
            ]

        compareBy :: ((MS, MS, MS) -> MS) -> MS -> MS -> Bool
        compareBy getElem ms_ msE = msE == getElem (ms2msTpl ms_)

        ms2msTpl :: MS -> (MS,MS,MS)
        ms2msTpl = foldr f ([], [], [])
            where
                f :: (Person, String) -> (MS, MS, MS) -> (MS, MS, MS)
                f (p, tStr) (xs', ys', zs') = 
                    let (a,b,c) = read tStr :: (a,b,c)
                        xsNew = (p, show a):xs'
                        ysNew = (p, show b):ys'
                        zsNew = (p, show c):zs'
                    in  (xsNew, ysNew, zsNew)

        -- ( [(A,"Brit"), (B,"Dane")], [(A,"Cat"), (B,"Dog")], [(B,"Water"), (A,"Beer")] )
        -- =>
        -- [ (A, "(Brit,Cat,Beer)"), (B, "(Dane,Dog,Water)") ]

        msTpl2ms :: (MS, MS, MS) -> MS
        msTpl2ms (xs, ys, zs) = map addString [A .. E]
            where
                addString :: Person -> (Person, String)
                addString p = 
                    let properties = show
                            (
                                propertyBy p xs :: a,
                                propertyBy p ys :: b,
                                propertyBy p zs :: c
                            )
                    in  (p, properties)


t10 = undefined
t11 = undefined
t13 = undefined
t15 = undefined
