import Constraints
import Data.List
import Data.Maybe
import Debug.Trace

{- 
links = l, 
halblinks = hl, 
mitte = m, 
halbrechts = hr, 
rechts = r
-}

positions :: [String]
positions = ["l", "hl", "m", "hr", "r"]



data Person = A | B | C | D | E 
    deriving(Show, Eq, Bounded, Enum)

data Pos = L | HL | M | HR | R 
    deriving(Show, Eq, Bounded, Enum, Read)

data Nat = Brit | Swed | Dane | Norwegian | German 
    deriving(Show, Eq, Bounded, Enum)

data Color = Red | Green | White | Blue | Yellow 
    deriving(Show, Eq, Bounded, Enum)

data Drink = Tea | Coffee | Milk | Beer | Water 
    deriving(Show, Eq, Bounded, Enum)

data Cigarete = Pallmall | Dunhill | Malboro | Winfield | Rothmanns 
    deriving(Show, Eq, Bounded, Enum)

data Animal = Dog | Cat | Bird | Horse | Fish 
    deriving(Show, Eq, Bounded, Enum)

ps :: [Person]
ps = [A .. E]

makeNodes :: [Nat] -> [Person] -> Node (Mapping String)
makeNodes nats ps = var "nat" (mkDom ps nats)

mkDom :: Show a => [Person] -> [a] -> Domain (Mapping String)
mkDom ps elems = map (\perm -> to5Tuple ps perm) (permutations elems) 

type Mapping a = [(Person, a)]

to5Tuple :: Show a => [Person] -> [a] -> Mapping String
to5Tuple (a:b:c:d:e:[]) (na:nb:nc:nd:ne:[]) =
    [(a, show na), (b, show nb), (c, show nc), (d, show nd), (e, show ne) ]
to5Tuple _ _ = error "Sorry dem has to be five."


personBy :: Show a => a -> Mapping String -> Person
personBy a = fst . fromJust . find (\(p,nat) -> (nat == show a))

propertyBy :: Read a => Person -> Mapping String -> a
propertyBy p = read . snd . fromJust . find (\(p', pos) -> p' == p)

c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15 :: Mapping String -> Mapping String -> Bool
c1 nm cm = personBy Brit nm == personBy Red cm
c2 nm cm = personBy Swed nm == personBy Dog cm
c3 nm cm = personBy Dane nm == personBy Tea cm

c4 cm ps = p1Pos /= (maxBound :: Pos) && succ p1Pos == p2Pos
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
c10 nm cm = undefined
c11 nm cm = undefined
c12 nm cm = personBy Winfield nm == personBy Beer cm
c13 nm cm = undefined

c14 nm cm = personBy German nm == personBy Rothmanns cm
c15 nm cm = undefined

net :: Net (Mapping String)
net = Net
        [   
            var "pos" (mkDom ps [L .. R]),
            var "nat" (mkDom ps [Brit .. German]),
            var "col" (mkDom ps [Red .. Yellow]),
            var "drk" (mkDom ps [Tea .. Water]),
            var "cgr" (mkDom ps [Pallmall .. Rothmanns]),
            var "ani" (mkDom ps [Dog .. Fish])
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
            -- 3facher constraint... complicated..
            -- mkConstraint "col" c10 "cgr" "Der Malboro-Raucher wohnt neben der Person mit der Katze.",
            -- mkConstraint "col" c11 "cgr" "Der Mann mit dem Pferd lebt neben der Person, die Dunhill raucht.",
            
            mkConstraint "cgr" c12 "drk" "Der Winfield-Raucher trinkt gern Bier.",
            -- mkConstraint "nat" c13 "col" "Der Norweger wohnt neben dem blauen Haus.",
            mkConstraint "nat" c14 "cgr" "Der Deutsche raucht Rothmanns."
            -- 3facher constraint... complicated..
            -- mkConstraint "col" c15 "cgr" "Der Malboro-Raucher hat einen Nachbarn, der Wasser trinkt."
            -- all dif
        ]

-- main = ac3 `with` fullLookAhead net
main :: IO ()
main = let sol = head $ solve net
       in  mapM_ (\(n, ls) -> putStrLn $ n ++ " -> " ++ show ls) sol

{-
colors = ["red", "green", "white", "blue", "yellow"]
nationality = ["brit", "swed", "dane", "norwegian", "german"]
drinks = ["tea", "coffee", "milk", "beer", "water"]
cigaretes = ["pallmall", "dunhill", "malboro", "winfield", "rothmanns"]
animals = ["dog", "cat", "bird", "horse", "fish"]
-}