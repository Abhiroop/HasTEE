module Denotation where

import qualified Data.Map as Map

type Var = String

supply :: [Var]
supply = map ((++) "v" . show) [0..]

fresh :: Int -> Var
fresh i = supply !! i

type Store = (Map.Map Var Int, Map.Map Var Int)

data Instruction
    = ServerConstant Int
    | Bind Instruction (Var -> Instruction)

type StoreT = (Int,Store) -> (Int,Store,Var)

eval :: Instruction -> StoreT
eval (ServerConstant x) = \(i, (tm, um)) ->
    let v = fresh i
    in (i+1, Map.insert v x tm, um, v)
eval (Bind ins f) = \(i, tm, um)) ->
    let (i',(tm', um'),v) = eval ins (i, tm, um)
    in eval (f (i, (tm', um'), v))
