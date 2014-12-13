
import Constraints

equals, mult2equals, smallerThan :: String -> String -> Constraint Int

equals x y = mkConstraint (==) (show x ++ " == " ++ show y) x y

mult2equals x y = mkConstraint (\x y -> x*2 == y) (show x ++ " *2 == " ++ show y) x y

smallerThan x y = mkConstraint (<) (show x ++ " < " ++ show y) x y

net :: Net Int
net = Net
        [
            ("V", [1,2,3,4]),
            ("X", [1,2,3,4]),
            ("Y", [1,2,3,4]),
            ("Z", [1,2,3,4])
        ]
        [
            "X" `equals` "V" ,
            "X" `mult2equals` "Z",
            "X" `smallerThan` "Y",
            "Y" `equals` "Z"
        ]


main = ac1 net

