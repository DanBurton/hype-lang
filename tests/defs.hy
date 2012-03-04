#module Nat

#Data

Nat
  S{pred : Nat}
  Z

Bool
  True
  False


#Definitions

plus : Nat -> Nat -> Nat
Z.plus m = m
n.plus Z = n
S{n}.plus m = S{n.plus m}

times : Nat -> Nat -> Nat
Z.times _ = Z
_.times Z = Z
S{n}.times m = m.plus (n.times m)

! : Bool -> Bool
! True = False
! False = True

