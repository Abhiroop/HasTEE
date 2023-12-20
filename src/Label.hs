module Label(module Label) where

import Data.Dynamic

class (Eq l, Show l, Read l, Typeable l) => Label l where
  lub       :: l -> l -> l
  glb       :: l -> l -> l
  canFlowTo :: l -> l -> Bool

infixl 5 `lub`, `glb`
infix 4 `canFlowTo`
