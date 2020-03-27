{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances#-}
module ASTInstances where
import qualified AST
import qualified GHC.Maybe
import Control.Applicative
import Control.Monad
import Data.Generics.Alloy
import qualified GHC.Types
instance (Alloy ((AST.Lambda)) (f :- ops) BaseOp, Alloy ((AST.MkClosure)) (f :- ops) BaseOp) =>
         Alloy ((AST.Abstraction)) BaseOp (f :- ops) where
  transform _ ops (AST.AClosure a0)
      =  AST.AClosure
     (transform ops BaseOp (a0))
  transform _ ops (AST.ALambda a0)
      =  AST.ALambda
     (transform ops BaseOp (a0))
instance () =>
         Alloy ((AST.Abstraction)) BaseOp BaseOp where
  transform _ _ v =  v
instance () =>
         Alloy ((AST.Abstraction)) (((AST.Abstraction)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.Abstraction)) r (((AST.App)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.App)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((AST.Assign)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.Assign)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((GHC.Types.Double)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((AST.Expr)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.Expr)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r ((Prelude.Integer) :- ops)) =>
         Alloy ((AST.Abstraction)) ((Prelude.Integer) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((AST.Lambda)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.Lambda)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((AST.Literal)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.Literal)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- ops)) =>
         Alloy ((AST.Abstraction)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((AST.MkClosure)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.MkClosure)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((AST.MkEnv)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.MkEnv)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (((AST.Var)) :- ops)) =>
         Alloy ((AST.Abstraction)) (((AST.Var)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.Abstraction)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Abstraction)) r (([[(GHC.Types.Char)]]) :- ops)) =>
         Alloy ((AST.Abstraction)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (AlloyA ((AST.Lambda)) (f :-* ops) BaseOpA, AlloyA ((AST.MkClosure)) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.Abstraction)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.AClosure a0)
      = pure AST.AClosure
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.ALambda a0)
      = pure AST.ALambda
   <*> (transformA ops BaseOpA (a0))
  transformM _ ops (AST.AClosure a0)
      = return AST.AClosure
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.ALambda a0)
      = return AST.ALambda
   `ap` (transformM ops BaseOpA (a0))
instance () =>
         AlloyA ((AST.Abstraction)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance () =>
         AlloyA ((AST.Abstraction)) (((AST.Abstraction)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.Abstraction)) r (((AST.App)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.App)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((AST.Assign)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.Assign)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((GHC.Types.Double)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((AST.Expr)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.Expr)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r ((Prelude.Integer) :-* ops)) =>
         AlloyA ((AST.Abstraction)) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((AST.Lambda)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.Lambda)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((AST.Literal)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.Literal)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((AST.MkClosure)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.MkClosure)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((AST.MkEnv)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.MkEnv)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (((AST.Var)) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (((AST.Var)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Abstraction)) r (([[(GHC.Types.Char)]]) :-* ops)) =>
         AlloyA ((AST.Abstraction)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyARoute ((AST.Lambda)) (f :-@ ops) BaseOpARoute, AlloyARoute ((AST.MkClosure)) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.Abstraction)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.AClosure a0 , rt)
      = pure AST.AClosure
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.AClosure b0) -> f b0 >>= (\b0 -> return (AST.AClosure b0)))))
  transformARoute _ ops (AST.ALambda a0 , rt)
      = pure AST.ALambda
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.ALambda b0) -> f b0 >>= (\b0 -> return (AST.ALambda b0)))))
  transformMRoute _ ops (AST.AClosure a0 , rt)
      = return AST.AClosure
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.AClosure b0) -> f b0 >>= (\b0 -> return (AST.AClosure b0)))))
  transformMRoute _ ops (AST.ALambda a0 , rt)
      = return AST.ALambda
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.ALambda b0) -> f b0 >>= (\b0 -> return (AST.ALambda b0)))))
instance () =>
         AlloyARoute ((AST.Abstraction)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance () =>
         AlloyARoute ((AST.Abstraction)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.App)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.App)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.Assign)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.Assign)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((GHC.Types.Double)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.Expr)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.Expr)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r ((Prelude.Integer) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.Lambda)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.Literal)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.Literal)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.MkClosure)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.MkEnv)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (((AST.Var)) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (((AST.Var)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Abstraction)) r (([[(GHC.Types.Char)]]) :-@ ops)) =>
         AlloyARoute ((AST.Abstraction)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (Alloy ((AST.Expr)) (f :- ops) BaseOp) =>
         Alloy ((AST.App)) BaseOp (f :- ops) where
  transform _ ops (AST.App a0 a1)
      =  AST.App
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
  transform _ ops (AST.AppC a0 a1)
      =  AST.AppC
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
instance () =>
         Alloy ((AST.App)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.App)) r (((AST.Abstraction)) :- ops)) =>
         Alloy ((AST.App)) (((AST.Abstraction)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance () =>
         Alloy ((AST.App)) (((AST.App)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.App)) r (((AST.Assign)) :- ops)) =>
         Alloy ((AST.App)) (((AST.Assign)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.App)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((GHC.Types.Double)) :- ops)) =>
         Alloy ((AST.App)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((AST.Expr)) :- ops)) =>
         Alloy ((AST.App)) (((AST.Expr)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r ((Prelude.Integer) :- ops)) =>
         Alloy ((AST.App)) ((Prelude.Integer) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((AST.Lambda)) :- ops)) =>
         Alloy ((AST.App)) (((AST.Lambda)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((AST.Literal)) :- ops)) =>
         Alloy ((AST.App)) (((AST.Literal)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- ops)) =>
         Alloy ((AST.App)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((AST.MkClosure)) :- ops)) =>
         Alloy ((AST.App)) (((AST.MkClosure)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((AST.MkEnv)) :- ops)) =>
         Alloy ((AST.App)) (((AST.MkEnv)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (((AST.Var)) :- ops)) =>
         Alloy ((AST.App)) (((AST.Var)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.App)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.App)) r (([[(GHC.Types.Char)]]) :- ops)) =>
         Alloy ((AST.App)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (AlloyA ((AST.Expr)) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.App)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.App a0 a1)
      = pure AST.App
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
  transformA _ ops (AST.AppC a0 a1)
      = pure AST.AppC
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
  transformM _ ops (AST.App a0 a1)
      = return AST.App
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
  transformM _ ops (AST.AppC a0 a1)
      = return AST.AppC
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
instance () =>
         AlloyA ((AST.App)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.App)) r (((AST.Abstraction)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.Abstraction)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance () =>
         AlloyA ((AST.App)) (((AST.App)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.App)) r (((AST.Assign)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.Assign)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.App)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((GHC.Types.Double)) :-* ops)) =>
         AlloyA ((AST.App)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((AST.Expr)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.Expr)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r ((Prelude.Integer) :-* ops)) =>
         AlloyA ((AST.App)) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((AST.Lambda)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.Lambda)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((AST.Literal)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.Literal)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* ops)) =>
         AlloyA ((AST.App)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((AST.MkClosure)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.MkClosure)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((AST.MkEnv)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.MkEnv)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (((AST.Var)) :-* ops)) =>
         AlloyA ((AST.App)) (((AST.Var)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.App)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.App)) r (([[(GHC.Types.Char)]]) :-* ops)) =>
         AlloyA ((AST.App)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyARoute ((AST.Expr)) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.App)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.App a0 a1 , rt)
      = pure AST.App
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.App b0 b1) -> f b0 >>= (\b0 -> return (AST.App b0 b1)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.App b0 b1) -> f b1 >>= (\b1 -> return (AST.App b0 b1)))))
  transformARoute _ ops (AST.AppC a0 a1 , rt)
      = pure AST.AppC
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.AppC b0 b1) -> f b0 >>= (\b0 -> return (AST.AppC b0 b1)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.AppC b0 b1) -> f b1 >>= (\b1 -> return (AST.AppC b0 b1)))))
  transformMRoute _ ops (AST.App a0 a1 , rt)
      = return AST.App
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.App b0 b1) -> f b0 >>= (\b0 -> return (AST.App b0 b1)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.App b0 b1) -> f b1 >>= (\b1 -> return (AST.App b0 b1)))))
  transformMRoute _ ops (AST.AppC a0 a1 , rt)
      = return AST.AppC
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.AppC b0 b1) -> f b0 >>= (\b0 -> return (AST.AppC b0 b1)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.AppC b0 b1) -> f b1 >>= (\b1 -> return (AST.AppC b0 b1)))))
instance () =>
         AlloyARoute ((AST.App)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.App)) r (((AST.Abstraction)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         AlloyARoute ((AST.App)) (((AST.App)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.App)) r (((AST.Assign)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.Assign)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((GHC.Types.Double)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((AST.Expr)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.Expr)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r ((Prelude.Integer) :-@ ops)) =>
         AlloyARoute ((AST.App)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((AST.Lambda)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((AST.Literal)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.Literal)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((AST.MkClosure)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((AST.MkEnv)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (((AST.Var)) :-@ ops)) =>
         AlloyARoute ((AST.App)) (((AST.Var)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.App)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.App)) r (([[(GHC.Types.Char)]]) :-@ ops)) =>
         AlloyARoute ((AST.App)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (Alloy ((AST.Expr)) (f :- ops) BaseOp, Alloy ([(GHC.Types.Char)]) (f :- ops) BaseOp) =>
         Alloy ((AST.Assign)) BaseOp (f :- ops) where
  transform _ ops (AST.Assign a0 a1)
      =  AST.Assign
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
instance () =>
         Alloy ((AST.Assign)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.Assign)) r (((AST.Abstraction)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.Abstraction)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((AST.App)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.App)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance () =>
         Alloy ((AST.Assign)) (((AST.Assign)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.Assign)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.Assign)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((GHC.Types.Double)) :- ops)) =>
         Alloy ((AST.Assign)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((AST.Expr)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.Expr)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r ((Prelude.Integer) :- ops)) =>
         Alloy ((AST.Assign)) ((Prelude.Integer) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((AST.Lambda)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.Lambda)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((AST.Literal)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.Literal)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- ops)) =>
         Alloy ((AST.Assign)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((AST.MkClosure)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.MkClosure)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((AST.MkEnv)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.MkEnv)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (((AST.Var)) :- ops)) =>
         Alloy ((AST.Assign)) (((AST.Var)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.Assign)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Assign)) r (([[(GHC.Types.Char)]]) :- ops)) =>
         Alloy ((AST.Assign)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (AlloyA ((AST.Expr)) (f :-* ops) BaseOpA, AlloyA ([(GHC.Types.Char)]) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.Assign)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.Assign a0 a1)
      = pure AST.Assign
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
  transformM _ ops (AST.Assign a0 a1)
      = return AST.Assign
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
instance () =>
         AlloyA ((AST.Assign)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.Assign)) r (((AST.Abstraction)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.Abstraction)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((AST.App)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.App)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance () =>
         AlloyA ((AST.Assign)) (((AST.Assign)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.Assign)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((GHC.Types.Double)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((AST.Expr)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.Expr)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r ((Prelude.Integer) :-* ops)) =>
         AlloyA ((AST.Assign)) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((AST.Lambda)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.Lambda)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((AST.Literal)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.Literal)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* ops)) =>
         AlloyA ((AST.Assign)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((AST.MkClosure)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.MkClosure)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((AST.MkEnv)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.MkEnv)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (((AST.Var)) :-* ops)) =>
         AlloyA ((AST.Assign)) (((AST.Var)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.Assign)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Assign)) r (([[(GHC.Types.Char)]]) :-* ops)) =>
         AlloyA ((AST.Assign)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyARoute ((AST.Expr)) (f :-@ ops) BaseOpARoute, AlloyARoute ([(GHC.Types.Char)]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.Assign)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.Assign a0 a1 , rt)
      = pure AST.Assign
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Assign b0 b1) -> f b0 >>= (\b0 -> return (AST.Assign b0 b1)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.Assign b0 b1) -> f b1 >>= (\b1 -> return (AST.Assign b0 b1)))))
  transformMRoute _ ops (AST.Assign a0 a1 , rt)
      = return AST.Assign
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Assign b0 b1) -> f b0 >>= (\b0 -> return (AST.Assign b0 b1)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.Assign b0 b1) -> f b1 >>= (\b1 -> return (AST.Assign b0 b1)))))
instance () =>
         AlloyARoute ((AST.Assign)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.Assign)) r (((AST.Abstraction)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((AST.App)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.App)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         AlloyARoute ((AST.Assign)) (((AST.Assign)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.Assign)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((GHC.Types.Double)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((AST.Expr)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.Expr)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r ((Prelude.Integer) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((AST.Lambda)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((AST.Literal)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.Literal)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((AST.MkClosure)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((AST.MkEnv)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (((AST.Var)) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (((AST.Var)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Assign)) r (([[(GHC.Types.Char)]]) :-@ ops)) =>
         AlloyARoute ((AST.Assign)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         Alloy ((GHC.Types.Char)) BaseOp (f :- ops) where
  transform _ _ v =  v
instance () =>
         Alloy ((GHC.Types.Char)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy ((GHC.Types.Char)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((GHC.Types.Double)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) ((Prelude.Integer) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (([(GHC.Types.Char)]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Char)) r ops) =>
         Alloy ((GHC.Types.Char)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         AlloyA ((GHC.Types.Char)) BaseOpA (f :-* ops) where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance () =>
         AlloyA ((GHC.Types.Char)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA ((GHC.Types.Char)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((GHC.Types.Double)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) ((Prelude.Integer) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Char)) r ops) =>
         AlloyA ((GHC.Types.Char)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyARoute ((GHC.Types.Char)) BaseOpARoute (f :-@ ops) where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance () =>
         AlloyARoute ((GHC.Types.Char)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute ((GHC.Types.Char)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) r ops) =>
         AlloyARoute ((GHC.Types.Char)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         Alloy ((GHC.Types.Double)) BaseOp (f :- ops) where
  transform _ _ v =  v
instance () =>
         Alloy ((GHC.Types.Double)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((GHC.Types.Char)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy ((GHC.Types.Double)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) ((Prelude.Integer) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (([(GHC.Types.Char)]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Types.Double)) r ops) =>
         Alloy ((GHC.Types.Double)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         AlloyA ((GHC.Types.Double)) BaseOpA (f :-* ops) where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance () =>
         AlloyA ((GHC.Types.Double)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((GHC.Types.Char)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA ((GHC.Types.Double)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) ((Prelude.Integer) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Types.Double)) r ops) =>
         AlloyA ((GHC.Types.Double)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyARoute ((GHC.Types.Double)) BaseOpARoute (f :-@ ops) where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance () =>
         AlloyARoute ((GHC.Types.Double)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute ((GHC.Types.Double)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Types.Double)) r ops) =>
         AlloyARoute ((GHC.Types.Double)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (Alloy ((AST.Abstraction)) (f :- ops) BaseOp, Alloy ((AST.App)) (f :- ops) BaseOp, Alloy ((AST.Assign)) (f :- ops) BaseOp, Alloy ((AST.Literal)) (f :- ops) BaseOp, Alloy ((AST.Var)) (f :- ops) BaseOp) =>
         Alloy ((AST.Expr)) BaseOp (f :- ops) where
  transform _ ops (AST.ELit a0)
      =  AST.ELit
     (transform ops BaseOp (a0))
  transform _ ops (AST.EAbs a0)
      =  AST.EAbs
     (transform ops BaseOp (a0))
  transform _ ops (AST.EApp a0)
      =  AST.EApp
     (transform ops BaseOp (a0))
  transform _ ops (AST.EVar a0)
      =  AST.EVar
     (transform ops BaseOp (a0))
  transform _ ops (AST.EAssign a0)
      =  AST.EAssign
     (transform ops BaseOp (a0))
instance () =>
         Alloy ((AST.Expr)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.Expr)) r (((AST.Abstraction)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.Abstraction)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((AST.App)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.App)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((AST.Assign)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.Assign)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.Expr)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((GHC.Types.Double)) :- ops)) =>
         Alloy ((AST.Expr)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance () =>
         Alloy ((AST.Expr)) (((AST.Expr)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.Expr)) r ((Prelude.Integer) :- ops)) =>
         Alloy ((AST.Expr)) ((Prelude.Integer) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((AST.Lambda)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.Lambda)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((AST.Literal)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.Literal)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- ops)) =>
         Alloy ((AST.Expr)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((AST.MkClosure)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.MkClosure)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((AST.MkEnv)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.MkEnv)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (((AST.Var)) :- ops)) =>
         Alloy ((AST.Expr)) (((AST.Var)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.Expr)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Expr)) r (([[(GHC.Types.Char)]]) :- ops)) =>
         Alloy ((AST.Expr)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (AlloyA ((AST.Abstraction)) (f :-* ops) BaseOpA, AlloyA ((AST.App)) (f :-* ops) BaseOpA, AlloyA ((AST.Assign)) (f :-* ops) BaseOpA, AlloyA ((AST.Literal)) (f :-* ops) BaseOpA, AlloyA ((AST.Var)) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.Expr)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.ELit a0)
      = pure AST.ELit
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.EAbs a0)
      = pure AST.EAbs
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.EApp a0)
      = pure AST.EApp
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.EVar a0)
      = pure AST.EVar
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.EAssign a0)
      = pure AST.EAssign
   <*> (transformA ops BaseOpA (a0))
  transformM _ ops (AST.ELit a0)
      = return AST.ELit
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.EAbs a0)
      = return AST.EAbs
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.EApp a0)
      = return AST.EApp
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.EVar a0)
      = return AST.EVar
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.EAssign a0)
      = return AST.EAssign
   `ap` (transformM ops BaseOpA (a0))
instance () =>
         AlloyA ((AST.Expr)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.Expr)) r (((AST.Abstraction)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.Abstraction)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((AST.App)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.App)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((AST.Assign)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.Assign)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((GHC.Types.Double)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance () =>
         AlloyA ((AST.Expr)) (((AST.Expr)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.Expr)) r ((Prelude.Integer) :-* ops)) =>
         AlloyA ((AST.Expr)) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((AST.Lambda)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.Lambda)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((AST.Literal)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.Literal)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* ops)) =>
         AlloyA ((AST.Expr)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((AST.MkClosure)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.MkClosure)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((AST.MkEnv)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.MkEnv)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (((AST.Var)) :-* ops)) =>
         AlloyA ((AST.Expr)) (((AST.Var)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.Expr)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Expr)) r (([[(GHC.Types.Char)]]) :-* ops)) =>
         AlloyA ((AST.Expr)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyARoute ((AST.Abstraction)) (f :-@ ops) BaseOpARoute, AlloyARoute ((AST.App)) (f :-@ ops) BaseOpARoute, AlloyARoute ((AST.Assign)) (f :-@ ops) BaseOpARoute, AlloyARoute ((AST.Literal)) (f :-@ ops) BaseOpARoute, AlloyARoute ((AST.Var)) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.Expr)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.ELit a0 , rt)
      = pure AST.ELit
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.ELit b0) -> f b0 >>= (\b0 -> return (AST.ELit b0)))))
  transformARoute _ ops (AST.EAbs a0 , rt)
      = pure AST.EAbs
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EAbs b0) -> f b0 >>= (\b0 -> return (AST.EAbs b0)))))
  transformARoute _ ops (AST.EApp a0 , rt)
      = pure AST.EApp
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EApp b0) -> f b0 >>= (\b0 -> return (AST.EApp b0)))))
  transformARoute _ ops (AST.EVar a0 , rt)
      = pure AST.EVar
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EVar b0) -> f b0 >>= (\b0 -> return (AST.EVar b0)))))
  transformARoute _ ops (AST.EAssign a0 , rt)
      = pure AST.EAssign
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EAssign b0) -> f b0 >>= (\b0 -> return (AST.EAssign b0)))))
  transformMRoute _ ops (AST.ELit a0 , rt)
      = return AST.ELit
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.ELit b0) -> f b0 >>= (\b0 -> return (AST.ELit b0)))))
  transformMRoute _ ops (AST.EAbs a0 , rt)
      = return AST.EAbs
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EAbs b0) -> f b0 >>= (\b0 -> return (AST.EAbs b0)))))
  transformMRoute _ ops (AST.EApp a0 , rt)
      = return AST.EApp
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EApp b0) -> f b0 >>= (\b0 -> return (AST.EApp b0)))))
  transformMRoute _ ops (AST.EVar a0 , rt)
      = return AST.EVar
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EVar b0) -> f b0 >>= (\b0 -> return (AST.EVar b0)))))
  transformMRoute _ ops (AST.EAssign a0 , rt)
      = return AST.EAssign
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EAssign b0) -> f b0 >>= (\b0 -> return (AST.EAssign b0)))))
instance () =>
         AlloyARoute ((AST.Expr)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.Expr)) r (((AST.Abstraction)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((AST.App)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.App)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((AST.Assign)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.Assign)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((GHC.Types.Double)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         AlloyARoute ((AST.Expr)) (((AST.Expr)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.Expr)) r ((Prelude.Integer) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((AST.Lambda)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((AST.Literal)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.Literal)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((AST.MkClosure)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((AST.MkEnv)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (((AST.Var)) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (((AST.Var)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Expr)) r (([[(GHC.Types.Char)]]) :-@ ops)) =>
         AlloyARoute ((AST.Expr)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         Alloy (Prelude.Integer) BaseOp (f :- ops) where
  transform _ _ v =  v
instance () =>
         Alloy (Prelude.Integer) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((GHC.Types.Char)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((GHC.Types.Double)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy (Prelude.Integer) ((Prelude.Integer) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (([(GHC.Types.Char)]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy (Prelude.Integer) r ops) =>
         Alloy (Prelude.Integer) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         AlloyA (Prelude.Integer) BaseOpA (f :-* ops) where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance () =>
         AlloyA (Prelude.Integer) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((GHC.Types.Char)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((GHC.Types.Double)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA (Prelude.Integer) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA (Prelude.Integer) r ops) =>
         AlloyA (Prelude.Integer) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyARoute (Prelude.Integer) BaseOpARoute (f :-@ ops) where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance () =>
         AlloyARoute (Prelude.Integer) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute (Prelude.Integer) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute (Prelude.Integer) r ops) =>
         AlloyARoute (Prelude.Integer) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (Alloy ((AST.Expr)) (f :- ops) BaseOp, Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (f :- ops) BaseOp, Alloy ([(GHC.Types.Char)]) (f :- ops) BaseOp) =>
         Alloy ((AST.Lambda)) BaseOp (f :- ops) where
  transform _ ops (AST.Lambda a0 a1 a2)
      =  AST.Lambda
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
     (transform ops BaseOp (a2))
instance () =>
         Alloy ((AST.Lambda)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.Lambda)) r (((AST.Abstraction)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.Abstraction)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((AST.App)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.App)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((AST.Assign)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.Assign)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.Lambda)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((GHC.Types.Double)) :- ops)) =>
         Alloy ((AST.Lambda)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((AST.Expr)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.Expr)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r ((Prelude.Integer) :- ops)) =>
         Alloy ((AST.Lambda)) ((Prelude.Integer) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance () =>
         Alloy ((AST.Lambda)) (((AST.Lambda)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.Lambda)) r (((AST.Literal)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.Literal)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- ops)) =>
         Alloy ((AST.Lambda)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((AST.MkClosure)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.MkClosure)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((AST.MkEnv)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.MkEnv)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (((AST.Var)) :- ops)) =>
         Alloy ((AST.Lambda)) (((AST.Var)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.Lambda)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Lambda)) r (([[(GHC.Types.Char)]]) :- ops)) =>
         Alloy ((AST.Lambda)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (AlloyA ((AST.Expr)) (f :-* ops) BaseOpA, AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (f :-* ops) BaseOpA, AlloyA ([(GHC.Types.Char)]) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.Lambda)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.Lambda a0 a1 a2)
      = pure AST.Lambda
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
   <*> (transformA ops BaseOpA (a2))
  transformM _ ops (AST.Lambda a0 a1 a2)
      = return AST.Lambda
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
   `ap` (transformM ops BaseOpA (a2))
instance () =>
         AlloyA ((AST.Lambda)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.Lambda)) r (((AST.Abstraction)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.Abstraction)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((AST.App)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.App)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((AST.Assign)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.Assign)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((GHC.Types.Double)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((AST.Expr)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.Expr)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r ((Prelude.Integer) :-* ops)) =>
         AlloyA ((AST.Lambda)) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance () =>
         AlloyA ((AST.Lambda)) (((AST.Lambda)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.Lambda)) r (((AST.Literal)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.Literal)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((AST.MkClosure)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.MkClosure)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((AST.MkEnv)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.MkEnv)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (((AST.Var)) :-* ops)) =>
         AlloyA ((AST.Lambda)) (((AST.Var)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.Lambda)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Lambda)) r (([[(GHC.Types.Char)]]) :-* ops)) =>
         AlloyA ((AST.Lambda)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyARoute ((AST.Expr)) (f :-@ ops) BaseOpARoute, AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (f :-@ ops) BaseOpARoute, AlloyARoute ([(GHC.Types.Char)]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.Lambda)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.Lambda a0 a1 a2 , rt)
      = pure AST.Lambda
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Lambda b0 b1 b2) -> f b0 >>= (\b0 -> return (AST.Lambda b0 b1 b2)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.Lambda b0 b1 b2) -> f b1 >>= (\b1 -> return (AST.Lambda b0 b1 b2)))))
   <*> (transformARoute ops BaseOpARoute (a2, rt @-> makeRoute [2] (\f (AST.Lambda b0 b1 b2) -> f b2 >>= (\b2 -> return (AST.Lambda b0 b1 b2)))))
  transformMRoute _ ops (AST.Lambda a0 a1 a2 , rt)
      = return AST.Lambda
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Lambda b0 b1 b2) -> f b0 >>= (\b0 -> return (AST.Lambda b0 b1 b2)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.Lambda b0 b1 b2) -> f b1 >>= (\b1 -> return (AST.Lambda b0 b1 b2)))))
   `ap` (transformMRoute ops BaseOpARoute (a2, rt @-> makeRoute [2] (\f (AST.Lambda b0 b1 b2) -> f b2 >>= (\b2 -> return (AST.Lambda b0 b1 b2)))))
instance () =>
         AlloyARoute ((AST.Lambda)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.Lambda)) r (((AST.Abstraction)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((AST.App)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.App)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((AST.Assign)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.Assign)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((GHC.Types.Double)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((AST.Expr)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.Expr)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r ((Prelude.Integer) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         AlloyARoute ((AST.Lambda)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.Lambda)) r (((AST.Literal)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.Literal)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((AST.MkClosure)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((AST.MkEnv)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (((AST.Var)) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (((AST.Var)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Lambda)) r (([[(GHC.Types.Char)]]) :-@ ops)) =>
         AlloyARoute ((AST.Lambda)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (Alloy ((GHC.Types.Char)) (f :- ops) BaseOp, Alloy ((GHC.Types.Double)) (f :- ops) BaseOp, Alloy (Prelude.Integer) (f :- ops) BaseOp) =>
         Alloy ((AST.Literal)) BaseOp (f :- ops) where
  transform _ ops (AST.Float a0)
      =  AST.Float
     (transform ops BaseOp (a0))
  transform _ ops (AST.Int a0)
      =  AST.Int
     (transform ops BaseOp (a0))
  transform _ ops (AST.Char a0)
      =  AST.Char
     (transform ops BaseOp (a0))
instance () =>
         Alloy ((AST.Literal)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.Literal)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Literal)) r (((GHC.Types.Double)) :- ops)) =>
         Alloy ((AST.Literal)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ((Prelude.Integer) :- ops)) =>
         Alloy ((AST.Literal)) ((Prelude.Integer) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy ((AST.Literal)) (((AST.Literal)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (([(GHC.Types.Char)]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Literal)) r ops) =>
         Alloy ((AST.Literal)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (AlloyA ((GHC.Types.Char)) (f :-* ops) BaseOpA, AlloyA ((GHC.Types.Double)) (f :-* ops) BaseOpA, AlloyA (Prelude.Integer) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.Literal)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.Float a0)
      = pure AST.Float
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.Int a0)
      = pure AST.Int
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.Char a0)
      = pure AST.Char
   <*> (transformA ops BaseOpA (a0))
  transformM _ ops (AST.Float a0)
      = return AST.Float
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.Int a0)
      = return AST.Int
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.Char a0)
      = return AST.Char
   `ap` (transformM ops BaseOpA (a0))
instance () =>
         AlloyA ((AST.Literal)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.Literal)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Literal)) r (((GHC.Types.Double)) :-* ops)) =>
         AlloyA ((AST.Literal)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ((Prelude.Integer) :-* ops)) =>
         AlloyA ((AST.Literal)) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA ((AST.Literal)) (((AST.Literal)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Literal)) r ops) =>
         AlloyA ((AST.Literal)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) (f :-@ ops) BaseOpARoute, AlloyARoute ((GHC.Types.Double)) (f :-@ ops) BaseOpARoute, AlloyARoute (Prelude.Integer) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.Literal)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.Float a0 , rt)
      = pure AST.Float
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Float b0) -> f b0 >>= (\b0 -> return (AST.Float b0)))))
  transformARoute _ ops (AST.Int a0 , rt)
      = pure AST.Int
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Int b0) -> f b0 >>= (\b0 -> return (AST.Int b0)))))
  transformARoute _ ops (AST.Char a0 , rt)
      = pure AST.Char
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Char b0) -> f b0 >>= (\b0 -> return (AST.Char b0)))))
  transformMRoute _ ops (AST.Float a0 , rt)
      = return AST.Float
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Float b0) -> f b0 >>= (\b0 -> return (AST.Float b0)))))
  transformMRoute _ ops (AST.Int a0 , rt)
      = return AST.Int
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Int b0) -> f b0 >>= (\b0 -> return (AST.Int b0)))))
  transformMRoute _ ops (AST.Char a0 , rt)
      = return AST.Char
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Char b0) -> f b0 >>= (\b0 -> return (AST.Char b0)))))
instance () =>
         AlloyARoute ((AST.Literal)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.Literal)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Literal)) r (((GHC.Types.Double)) :-@ ops)) =>
         AlloyARoute ((AST.Literal)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ((Prelude.Integer) :-@ ops)) =>
         AlloyARoute ((AST.Literal)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute ((AST.Literal)) (((AST.Literal)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Literal)) r ops) =>
         AlloyARoute ((AST.Literal)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (Alloy ([(GHC.Types.Char)]) (f :- ops) BaseOp) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) BaseOp (f :- ops) where
  transform _ _ (GHC.Maybe.Nothing)
      =  GHC.Maybe.Nothing
  transform _ ops (GHC.Maybe.Just a0)
      =  GHC.Maybe.Just
     (transform ops BaseOp (a0))
instance () =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Types.Double)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) ((Prelude.Integer) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         Alloy ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) (f :-* ops) BaseOpA) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) BaseOpA (f :-* ops) where
  transformA _ _ (GHC.Maybe.Nothing)
      = pure GHC.Maybe.Nothing
  transformA _ ops (GHC.Maybe.Just a0)
      = pure GHC.Maybe.Just
   <*> (transformA ops BaseOpA (a0))
  transformM _ _ (GHC.Maybe.Nothing)
      = return GHC.Maybe.Nothing
  transformM _ ops (GHC.Maybe.Just a0)
      = return GHC.Maybe.Just
   `ap` (transformM ops BaseOpA (a0))
instance () =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Types.Double)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) ((Prelude.Integer) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyA ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) BaseOpARoute (f :-@ ops) where
  transformARoute _ _ (GHC.Maybe.Nothing , _)
      = pure GHC.Maybe.Nothing
  transformARoute _ ops (GHC.Maybe.Just a0 , rt)
      = pure GHC.Maybe.Just
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (GHC.Maybe.Just b0) -> f b0 >>= (\b0 -> return (GHC.Maybe.Just b0)))))
  transformMRoute _ _ (GHC.Maybe.Nothing , _)
      = return GHC.Maybe.Nothing
  transformMRoute _ ops (GHC.Maybe.Just a0 , rt)
      = return GHC.Maybe.Just
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (GHC.Maybe.Just b0) -> f b0 >>= (\b0 -> return (GHC.Maybe.Just b0)))))
instance () =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) ((Prelude.Integer) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) r ops) =>
         AlloyARoute ((GHC.Maybe.Maybe [(GHC.Types.Char)])) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (Alloy ((AST.Lambda)) (f :- ops) BaseOp, Alloy ((AST.MkEnv)) (f :- ops) BaseOp, Alloy ([(GHC.Types.Char)]) (f :- ops) BaseOp) =>
         Alloy ((AST.MkClosure)) BaseOp (f :- ops) where
  transform _ ops (AST.MkClosure a0 a1 a2)
      =  AST.MkClosure
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
     (transform ops BaseOp (a2))
instance () =>
         Alloy ((AST.MkClosure)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.MkClosure)) r (((AST.Abstraction)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.Abstraction)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((AST.App)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.App)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((AST.Assign)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.Assign)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((GHC.Types.Double)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((GHC.Types.Double)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((AST.Expr)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.Expr)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r ((Prelude.Integer) :- ops)) =>
         Alloy ((AST.MkClosure)) ((Prelude.Integer) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((AST.Lambda)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.Lambda)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((AST.Literal)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.Literal)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- ops)) =>
         Alloy ((AST.MkClosure)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance () =>
         Alloy ((AST.MkClosure)) (((AST.MkClosure)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.MkClosure)) r (((AST.MkEnv)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.MkEnv)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (((AST.Var)) :- ops)) =>
         Alloy ((AST.MkClosure)) (((AST.Var)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.MkClosure)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkClosure)) r (([[(GHC.Types.Char)]]) :- ops)) =>
         Alloy ((AST.MkClosure)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (AlloyA ((AST.Lambda)) (f :-* ops) BaseOpA, AlloyA ((AST.MkEnv)) (f :-* ops) BaseOpA, AlloyA ([(GHC.Types.Char)]) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.MkClosure)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.MkClosure a0 a1 a2)
      = pure AST.MkClosure
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
   <*> (transformA ops BaseOpA (a2))
  transformM _ ops (AST.MkClosure a0 a1 a2)
      = return AST.MkClosure
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
   `ap` (transformM ops BaseOpA (a2))
instance () =>
         AlloyA ((AST.MkClosure)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.MkClosure)) r (((AST.Abstraction)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.Abstraction)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((AST.App)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.App)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((AST.Assign)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.Assign)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((GHC.Types.Double)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((GHC.Types.Double)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((AST.Expr)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.Expr)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r ((Prelude.Integer) :-* ops)) =>
         AlloyA ((AST.MkClosure)) ((Prelude.Integer) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((AST.Lambda)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.Lambda)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((AST.Literal)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.Literal)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance () =>
         AlloyA ((AST.MkClosure)) (((AST.MkClosure)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.MkClosure)) r (((AST.MkEnv)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.MkEnv)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (((AST.Var)) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (((AST.Var)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkClosure)) r (([[(GHC.Types.Char)]]) :-* ops)) =>
         AlloyA ((AST.MkClosure)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyARoute ((AST.Lambda)) (f :-@ ops) BaseOpARoute, AlloyARoute ((AST.MkEnv)) (f :-@ ops) BaseOpARoute, AlloyARoute ([(GHC.Types.Char)]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.MkClosure)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.MkClosure a0 a1 a2 , rt)
      = pure AST.MkClosure
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.MkClosure b0 b1 b2) -> f b0 >>= (\b0 -> return (AST.MkClosure b0 b1 b2)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.MkClosure b0 b1 b2) -> f b1 >>= (\b1 -> return (AST.MkClosure b0 b1 b2)))))
   <*> (transformARoute ops BaseOpARoute (a2, rt @-> makeRoute [2] (\f (AST.MkClosure b0 b1 b2) -> f b2 >>= (\b2 -> return (AST.MkClosure b0 b1 b2)))))
  transformMRoute _ ops (AST.MkClosure a0 a1 a2 , rt)
      = return AST.MkClosure
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.MkClosure b0 b1 b2) -> f b0 >>= (\b0 -> return (AST.MkClosure b0 b1 b2)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.MkClosure b0 b1 b2) -> f b1 >>= (\b1 -> return (AST.MkClosure b0 b1 b2)))))
   `ap` (transformMRoute ops BaseOpARoute (a2, rt @-> makeRoute [2] (\f (AST.MkClosure b0 b1 b2) -> f b2 >>= (\b2 -> return (AST.MkClosure b0 b1 b2)))))
instance () =>
         AlloyARoute ((AST.MkClosure)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.MkClosure)) r (((AST.Abstraction)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((AST.App)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.App)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((AST.Assign)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.Assign)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((GHC.Types.Double)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((AST.Expr)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.Expr)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r ((Prelude.Integer) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((AST.Lambda)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((AST.Literal)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.Literal)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         AlloyARoute ((AST.MkClosure)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.MkClosure)) r (((AST.MkEnv)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (((AST.Var)) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (((AST.Var)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkClosure)) r (([[(GHC.Types.Char)]]) :-@ ops)) =>
         AlloyARoute ((AST.MkClosure)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (Alloy ([[(GHC.Types.Char)]]) (f :- ops) BaseOp) =>
         Alloy ((AST.MkEnv)) BaseOp (f :- ops) where
  transform _ ops (AST.MkEnv a0)
      =  AST.MkEnv
     (transform ops BaseOp (a0))
instance () =>
         Alloy ((AST.MkEnv)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.MkEnv)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((GHC.Types.Double)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) ((Prelude.Integer) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy ((AST.MkEnv)) (((AST.MkEnv)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.MkEnv)) r ops) =>
         Alloy ((AST.MkEnv)) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.MkEnv)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.MkEnv)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.MkEnv)) r (([[(GHC.Types.Char)]]) :- ops)) =>
         Alloy ((AST.MkEnv)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (AlloyA ([[(GHC.Types.Char)]]) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.MkEnv)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.MkEnv a0)
      = pure AST.MkEnv
   <*> (transformA ops BaseOpA (a0))
  transformM _ ops (AST.MkEnv a0)
      = return AST.MkEnv
   `ap` (transformM ops BaseOpA (a0))
instance () =>
         AlloyA ((AST.MkEnv)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.MkEnv)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((GHC.Types.Double)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) ((Prelude.Integer) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA ((AST.MkEnv)) (((AST.MkEnv)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.MkEnv)) r ops) =>
         AlloyA ((AST.MkEnv)) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.MkEnv)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.MkEnv)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.MkEnv)) r (([[(GHC.Types.Char)]]) :-* ops)) =>
         AlloyA ((AST.MkEnv)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.MkEnv)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.MkEnv a0 , rt)
      = pure AST.MkEnv
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.MkEnv b0) -> f b0 >>= (\b0 -> return (AST.MkEnv b0)))))
  transformMRoute _ ops (AST.MkEnv a0 , rt)
      = return AST.MkEnv
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.MkEnv b0) -> f b0 >>= (\b0 -> return (AST.MkEnv b0)))))
instance () =>
         AlloyARoute ((AST.MkEnv)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.MkEnv)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute ((AST.MkEnv)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.MkEnv)) r ops) =>
         AlloyARoute ((AST.MkEnv)) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.MkEnv)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.MkEnv)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.MkEnv)) r (([[(GHC.Types.Char)]]) :-@ ops)) =>
         AlloyARoute ((AST.MkEnv)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (Alloy ([(GHC.Types.Char)]) (f :- ops) BaseOp) =>
         Alloy ((AST.Var)) BaseOp (f :- ops) where
  transform _ ops (AST.Var a0)
      =  AST.Var
     (transform ops BaseOp (a0))
  transform _ ops (AST.EnvRef a0 a1)
      =  AST.EnvRef
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
instance () =>
         Alloy ((AST.Var)) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ((AST.Var)) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((GHC.Types.Double)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) ((Prelude.Integer) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy ((AST.Var)) (((AST.Var)) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ((AST.Var)) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ((AST.Var)) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ((AST.Var)) r ops) =>
         Alloy ((AST.Var)) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) (f :-* ops) BaseOpA) =>
         AlloyA ((AST.Var)) BaseOpA (f :-* ops) where
  transformA _ ops (AST.Var a0)
      = pure AST.Var
   <*> (transformA ops BaseOpA (a0))
  transformA _ ops (AST.EnvRef a0 a1)
      = pure AST.EnvRef
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
  transformM _ ops (AST.Var a0)
      = return AST.Var
   `ap` (transformM ops BaseOpA (a0))
  transformM _ ops (AST.EnvRef a0 a1)
      = return AST.EnvRef
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
instance () =>
         AlloyA ((AST.Var)) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ((AST.Var)) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((GHC.Types.Double)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) ((Prelude.Integer) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA ((AST.Var)) (((AST.Var)) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ((AST.Var)) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ((AST.Var)) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ((AST.Var)) r ops) =>
         AlloyA ((AST.Var)) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ((AST.Var)) BaseOpARoute (f :-@ ops) where
  transformARoute _ ops (AST.Var a0 , rt)
      = pure AST.Var
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Var b0) -> f b0 >>= (\b0 -> return (AST.Var b0)))))
  transformARoute _ ops (AST.EnvRef a0 a1 , rt)
      = pure AST.EnvRef
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EnvRef b0 b1) -> f b0 >>= (\b0 -> return (AST.EnvRef b0 b1)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.EnvRef b0 b1) -> f b1 >>= (\b1 -> return (AST.EnvRef b0 b1)))))
  transformMRoute _ ops (AST.Var a0 , rt)
      = return AST.Var
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.Var b0) -> f b0 >>= (\b0 -> return (AST.Var b0)))))
  transformMRoute _ ops (AST.EnvRef a0 a1 , rt)
      = return AST.EnvRef
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f (AST.EnvRef b0 b1) -> f b0 >>= (\b0 -> return (AST.EnvRef b0 b1)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f (AST.EnvRef b0 b1) -> f b1 >>= (\b1 -> return (AST.EnvRef b0 b1)))))
instance () =>
         AlloyARoute ((AST.Var)) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ((AST.Var)) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) ((Prelude.Integer) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute ((AST.Var)) (((AST.Var)) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ((AST.Var)) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ((AST.Var)) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ((AST.Var)) r ops) =>
         AlloyARoute ((AST.Var)) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (Alloy ((GHC.Types.Char)) (f :- ops) BaseOp, Alloy ([(GHC.Types.Char)]) (f :- ops) BaseOp) =>
         Alloy ([(GHC.Types.Char)]) BaseOp (f :- ops) where
  transform _ _ ([])
      =  []
  transform _ ops ((:) a0 a1)
      =  (:)
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
instance () =>
         Alloy ([(GHC.Types.Char)]) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ([(GHC.Types.Char)]) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((GHC.Types.Double)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) ((Prelude.Integer) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance () =>
         Alloy ([(GHC.Types.Char)]) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (Alloy ([(GHC.Types.Char)]) r ops) =>
         Alloy ([(GHC.Types.Char)]) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (AlloyA ((GHC.Types.Char)) (f :-* ops) BaseOpA, AlloyA ([(GHC.Types.Char)]) (f :-* ops) BaseOpA) =>
         AlloyA ([(GHC.Types.Char)]) BaseOpA (f :-* ops) where
  transformA _ _ ([])
      = pure []
  transformA _ ops ((:) a0 a1)
      = pure (:)
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
  transformM _ _ ([])
      = return []
  transformM _ ops ((:) a0 a1)
      = return (:)
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
instance () =>
         AlloyA ([(GHC.Types.Char)]) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ([(GHC.Types.Char)]) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((GHC.Types.Double)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) ((Prelude.Integer) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance () =>
         AlloyA ([(GHC.Types.Char)]) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyA ([(GHC.Types.Char)]) r ops) =>
         AlloyA ([(GHC.Types.Char)]) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyARoute ((GHC.Types.Char)) (f :-@ ops) BaseOpARoute, AlloyARoute ([(GHC.Types.Char)]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ([(GHC.Types.Char)]) BaseOpARoute (f :-@ ops) where
  transformARoute _ _ ([] , _)
      = pure []
  transformARoute _ ops ((:) a0 a1 , rt)
      = pure (:)
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f ((:) b0 b1) -> f b0 >>= (\b0 -> return ((:) b0 b1)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f ((:) b0 b1) -> f b1 >>= (\b1 -> return ((:) b0 b1)))))
  transformMRoute _ _ ([] , _)
      = return []
  transformMRoute _ ops ((:) a0 a1 , rt)
      = return (:)
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f ((:) b0 b1) -> f b0 >>= (\b0 -> return ((:) b0 b1)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f ((:) b0 b1) -> f b1 >>= (\b1 -> return ((:) b0 b1)))))
instance () =>
         AlloyARoute ([(GHC.Types.Char)]) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ([(GHC.Types.Char)]) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) ((Prelude.Integer) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance () =>
         AlloyARoute ([(GHC.Types.Char)]) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
instance (AlloyARoute ([(GHC.Types.Char)]) r ops) =>
         AlloyARoute ([(GHC.Types.Char)]) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (Alloy ([(GHC.Types.Char)]) (f :- ops) BaseOp, Alloy ([[(GHC.Types.Char)]]) (f :- ops) BaseOp) =>
         Alloy ([[(GHC.Types.Char)]]) BaseOp (f :- ops) where
  transform _ _ ([])
      =  []
  transform _ ops ((:) a0 a1)
      =  (:)
     (transform ops BaseOp (a0))
     (transform ops BaseOp (a1))
instance () =>
         Alloy ([[(GHC.Types.Char)]]) BaseOp BaseOp where
  transform _ _ v =  v
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.Abstraction)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.App)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.Assign)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r (((GHC.Types.Char)) :- ops)) =>
         Alloy ([[(GHC.Types.Char)]]) (((GHC.Types.Char)) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((GHC.Types.Double)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.Expr)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) ((Prelude.Integer) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.Lambda)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.Literal)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.MkClosure)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.MkEnv)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r ops) =>
         Alloy ([[(GHC.Types.Char)]]) (((AST.Var)) :- r) ops where
  transform (_ :- rest) ops vr = transform rest ops vr
instance (Alloy ([[(GHC.Types.Char)]]) r (([(GHC.Types.Char)]) :- ops)) =>
         Alloy ([[(GHC.Types.Char)]]) (([(GHC.Types.Char)]) :- r) ops where
  transform (f :- rest) ops vr = transform rest (f :- ops) vr
instance () =>
         Alloy ([[(GHC.Types.Char)]]) (([[(GHC.Types.Char)]]) :- r) ops where
  transform (f :- _) _ vr = f vr
instance (AlloyA ([(GHC.Types.Char)]) (f :-* ops) BaseOpA, AlloyA ([[(GHC.Types.Char)]]) (f :-* ops) BaseOpA) =>
         AlloyA ([[(GHC.Types.Char)]]) BaseOpA (f :-* ops) where
  transformA _ _ ([])
      = pure []
  transformA _ ops ((:) a0 a1)
      = pure (:)
   <*> (transformA ops BaseOpA (a0))
   <*> (transformA ops BaseOpA (a1))
  transformM _ _ ([])
      = return []
  transformM _ ops ((:) a0 a1)
      = return (:)
   `ap` (transformM ops BaseOpA (a0))
   `ap` (transformM ops BaseOpA (a1))
instance () =>
         AlloyA ([[(GHC.Types.Char)]]) BaseOpA BaseOpA where
  transformA _ _ v = pure v
  transformM _ _ v = return v
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.Abstraction)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.App)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.Assign)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r (((GHC.Types.Char)) :-* ops)) =>
         AlloyA ([[(GHC.Types.Char)]]) (((GHC.Types.Char)) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((GHC.Types.Double)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.Expr)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) ((Prelude.Integer) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.Lambda)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.Literal)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.MkClosure)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.MkEnv)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r ops) =>
         AlloyA ([[(GHC.Types.Char)]]) (((AST.Var)) :-* r) ops where
  transformA (_ :-* rest) ops vr = transformA rest ops vr
  transformM (_ :-* rest) ops vr = transformM rest ops vr
instance (AlloyA ([[(GHC.Types.Char)]]) r (([(GHC.Types.Char)]) :-* ops)) =>
         AlloyA ([[(GHC.Types.Char)]]) (([(GHC.Types.Char)]) :-* r) ops where
  transformA (f :-* rest) ops vr = transformA rest (f :-* ops) vr
  transformM (f :-* rest) ops vr = transformM rest (f :-* ops) vr
instance () =>
         AlloyA ([[(GHC.Types.Char)]]) (([[(GHC.Types.Char)]]) :-* r) ops where
  transformA (f :-* _) _ vr = f vr
  transformM (f :-* _) _ vr = f vr
instance (AlloyARoute ([(GHC.Types.Char)]) (f :-@ ops) BaseOpARoute, AlloyARoute ([[(GHC.Types.Char)]]) (f :-@ ops) BaseOpARoute) =>
         AlloyARoute ([[(GHC.Types.Char)]]) BaseOpARoute (f :-@ ops) where
  transformARoute _ _ ([] , _)
      = pure []
  transformARoute _ ops ((:) a0 a1 , rt)
      = pure (:)
   <*> (transformARoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f ((:) b0 b1) -> f b0 >>= (\b0 -> return ((:) b0 b1)))))
   <*> (transformARoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f ((:) b0 b1) -> f b1 >>= (\b1 -> return ((:) b0 b1)))))
  transformMRoute _ _ ([] , _)
      = return []
  transformMRoute _ ops ((:) a0 a1 , rt)
      = return (:)
   `ap` (transformMRoute ops BaseOpARoute (a0, rt @-> makeRoute [0] (\f ((:) b0 b1) -> f b0 >>= (\b0 -> return ((:) b0 b1)))))
   `ap` (transformMRoute ops BaseOpARoute (a1, rt @-> makeRoute [1] (\f ((:) b0 b1) -> f b1 >>= (\b1 -> return ((:) b0 b1)))))
instance () =>
         AlloyARoute ([[(GHC.Types.Char)]]) BaseOpARoute BaseOpARoute where
  transformARoute _ _ (v, _) = pure v
  transformMRoute _ _ (v, _) = return v
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.Abstraction)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.App)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.Assign)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r (((GHC.Types.Char)) :-@ ops)) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((GHC.Types.Char)) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((GHC.Types.Double)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.Expr)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) ((Prelude.Integer) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.Lambda)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.Literal)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((GHC.Maybe.Maybe [(GHC.Types.Char)])) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.MkClosure)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.MkEnv)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r ops) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (((AST.Var)) :-@ r) ops where
  transformARoute (_ :-@ rest) ops vr = transformARoute rest ops vr
  transformMRoute (_ :-@ rest) ops vr = transformMRoute rest ops vr
instance (AlloyARoute ([[(GHC.Types.Char)]]) r (([(GHC.Types.Char)]) :-@ ops)) =>
         AlloyARoute ([[(GHC.Types.Char)]]) (([(GHC.Types.Char)]) :-@ r) ops where
  transformARoute (f :-@ rest) ops vr = transformARoute rest (f :-@ ops) vr
  transformMRoute (f :-@ rest) ops vr = transformMRoute rest (f :-@ ops) vr
instance () =>
         AlloyARoute ([[(GHC.Types.Char)]]) (([[(GHC.Types.Char)]]) :-@ r) ops where
  transformARoute (f :-@ _) _ vr = f vr
  transformMRoute (f :-@ _) _ vr = f vr
