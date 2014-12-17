
module Constraints (
        Net(..), Constraint, Node(..),
        Domain, NodeName, nodeName,
        mkConstraint, findNode, var,
        ac3, solve
    ) where

import Types
import ArcConsistency
import Solving

