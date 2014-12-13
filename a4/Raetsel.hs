
import Constraints

net :: Net Int
net = Net
        [
            ("V", [1,2,3,4]),
            ("X", [1,2,3,4]),
            ("Y", [1,2,3,4]),
            ("Z", [1,2,3,4])
        ]
        [
            mkConstraint "X" (==) "V"               "X = V",
            mkConstraint "X" (\x z -> x*2 == z) "Z" "X *2 = Z",
            mkConstraint "X" (<) "Y"                "X < Y",
            mkConstraint "Y" (==) "Z"               "Y = Z"
        ]


main = ac1 net

