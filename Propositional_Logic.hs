module Propositional_Logic where

data Prop = T 
          | F 
          | Atom String 
          | Not Prop 
          | And Prop Prop 
          | Or Prop Prop 
          | Implies Prop Prop 
          | Iff Prop Prop 
          deriving (Show, Eq)

andIntroduction :: Prop -> Prop -> Prop
andIntroduction p q = And p q

andEliminationL :: Prop -> Prop
andElimination (And p _) = p 

andEliminationR :: Prop -> Prop
andElimination (And _ q) = q

trueIntroduction ::  Prop
trueIntroduction = T 

falseIntroduction :: Prop -> Prop
falseIntroduction _ = F

orIntroductionR :: Prop -> Prop -> Prop
orIntroduction p = Or p q

orIntroductionL :: Prop -> Prop -> Prop
orIntroduction q = Or p q

orElimination :: Prop -> (Prop -> Prop) -> (Prop -> Prop) -> Prop
orElimination (Or p q) leftProof rightProof = And (leftProof p) (rightProof q)

implicationIntroduction :: (Prop -> Prop) -> Prop -> Prop
implicationIntroduction proofq p = Implies p (proofq p)

implicationElimination :: Prop -> Prop -> Prop
implicationElimination (Implies p q) p = q

notIntroduction:: (Prop -> Prop) -> Prop -> Prop
notIntroduction notProof F = Not p

notElimination :: Prop -> Prop -> Prop
notElimination p (Not p) = F 

lawOfExcludedMiddle :: Prop
lawOfExcludedMiddle = (Or p (Not q))

