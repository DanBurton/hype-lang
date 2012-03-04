module Core

List A
  Cons{head : A, tail : List A}
  Nil

dynamic dispatch fold on T : *

fold on List A : (A -> Z -> Z) -> Z -> List A -> Z
Cons{x, xs}.fold c z = c x (xs.fold c z)
Nil.fold c z = z

Bool
  True
  False

fold on Bool : A -> A -> Bool -> A
True.fold t _  = t
False.fold _ f = f

Nat
  S{pred : Nat}
  Z

fold on Nat : (A -> A) -> A -> Nat -> A
S{n}.fold s z = s (n.fold s z)
Z.fold s z = z


Foo A
  F{x : Bar} : Foo Baz

fold on Foo Baz : (Bar -> A) -> A
F{x}.fold f = f x

// macro #wrapper X Y (
// X
//   X{Y}
//
// wrap = X
// X{y}.unwrap = y
// )

infix .
. : (b -> c) -> (a -> b) -> c
(f . g) x = f (g x)

// #wrapper Baz Nat

// fold on Baz : (Nat -> A) -> A
// fold f = f . unwrap

succ : Nat -> Nat
succ = S

infix $
unique $ : (a -> b) -> a -> b
f $ x = f x

infix :
x : xs = Cons{x, xs}
// (:) = Cons

infix *
Z * _ = Z
_ * Z = Z
S{n} * m = m + (n * m)

infix +
Z + m = m
n + Z = n
S{n} + m = S{n + m}

0 = Z
1 = S{Z}

factorial : Nat -> Nat
Z.factorial = 1
n.factorial = n * n.pred.factorial


fib : Nat -> Nat
Z.fib = 0
S{Z}.fib = 1
n.fib = (n - 2).fib + (n - 1).fib


dynamic dispatch drop on L : * -> *


drop on List : Nat -> List A -> List A
xs.drop Z = xs
Nil.drop _ = Nil
Cons{_, xs}.drop S{n} = xs.drop n


fibs : List Nat
fibs = 0 : 1 : zipWith (+) (fibs.drop 1) fibs

! : Bool -> Bool
! true = false
! false = true

infix ++
Nil ++ ys = ys
Cons{x, xs} ++ ys = Cons{x, xs ++ ys}

Nil >>= _ = Nil
Cons{x,xs} >>= f = f x ++ (xs >>= f)

x.join = x >>= id

interface Functor f : * -> * given a : *, b : *
  fmap : (a -> b) -> f a -> f b

interface Pure a : *
  pure : a

interface Applicative f : * -> * given a : *, b : *, Functor f, Pure (f a)
  <*> : f (a -> b) -> f a -> f b

id : A -> A  
x.id = x

interface Monad m requires Applicative m
  join on m (m a) : m (m a) -> m a
  mma.join = mma >>= id

  infix >>=
  >>= on m a : m a -> (a -> m b) -> m b
  x >>= f = join (fmap f x)

