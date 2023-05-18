(?) = see later

```rust
// nice
val flip : forall <a, b, c> . (a -> b -> c) -> b -> a -> c
def flip = (f, a, b) => f(b, a)

// nice (forall?)
val flip : (a -> b -> c) -> b -> a -> c
def flip = (f, a, b) => f(b, a)

// nice (?)
val flip : (a -> b -> c) -> b -> a -> c
let flip = (f, a, b) => f(b, a)

// nice
flip : (a -> b -> c) -> b -> a -> c
flip = (f, a, b) => f(b, a)

// weird
flip: (a -> b -> c) -> b -> a -> c = (f, a, b) => f(b, a)

// weird
flip (f: (a -> b -> c)) (a: b) (b: a): a => f(b, a)

// weird
flip: a = (f: (a -> b -> c)) => (a: b) => (b: a) => f(b, a)

// dot?
length : forall <a> . list<a> -> int

// weird
length : forall <a> => list<a> -> int

// weird
length <a> : list<a> -> int

// weird
flip <a, b, c> : (a -> b -> c) -> b -> a -> c

// weird (?)
flip : <a, b, c> . (a -> b -> c) -> b -> a -> c
flip : forall <a, b, c> . (a -> b -> c) -> b -> a -> c

// looks good?.
length [a] : list(a) -> int
// looks good.
length : forall {a} => list(a) -> int
// looks good.
length : forall [a] => list(a) -> int

// acceptable
swap : forall {a, b} => (a, b) -> (b, a)
swap = ((x, y)) => (y, x);

pair : forall {a, b} => (a, b)
pair = lambda (a, b) => (a, b)

pair : forall {a, b} => Pair(a, b)
pair = lambda (a, b) => Pair(a, b)

id : forall {a}. a
id = lambda a => a

id = lambda x => x
// id : forall {a} => (a : Linear) -> (a : Linear)

apply (x : _) = x
// forall {a} => a -> a

apply (x : _) : Int = x
// Int -> Int

id : forall {a} => a -> a
id = lambda (x) => x

id : forall {a} => a -> a
id = lambda (x) => x

(id 1, id 'a')

((id(id)), 10)

id : Int -> Int
id x = x

// annotations?
one : Int
one = 1

// parameter annotation : ok
id = lambda (x : Int) => x
// expression annotation : ok
((id = lambda x => x) : Int -> Int)
// toplevel expression annotation : hmm no.
((id x = x) : Int -> Int)

// linear?
// cute, but keyboard limited?
lambda (x : a~2) => x
// bruh
lambda (x : a#2) => x
// bruh
lambda (x : a@2) => x
// bruh
lambda (x : a%2) => x
// infer
lambda (x : Int [1]) => x
// let!
lambda (x : Int) =>
  let! a = x + 1 in a // let a : forall a. a [1]
  let! b = a + 1 in b // let b : forall b. b [1]
lambda (x : Int) =>
  let! a = x + 1 in
  a + a
// weird
lambda (x : Int) =>
  let a = x + 1 in
  a + a
// weird
lambda (x : Int) =>
  let a : ![2] = x + 1 in
  a + a

// ranks

empty : List(forall {a} => a -> a)
empty = []

ids : List(forall {a} => a -> a)
ids = (lambda x => x) :: empty

foo : (forall {a} => a -> a) * (forall {a} => a -> int)
foo = (lambda x => x, lambda x => 1)

fst (x, y) = x
snd (x, y) = y

apply = fst(foo, 1) + snd(foo, ())

magic : forall a => (a * (a -> Int))
magic = lambda x =>
  case x
  | (x, f) -> f x

(id : forall {a} => a -> a)
(id 5, id ())

// looks interesting
x : forall { a : *, b : *, n : Nat } // Nat : *
  => (a -> b) [n] // function used `n` times
  -> vec(a, n) // vec `a` with length `n`
  -> vec(b, n) // vec `b` with length `n`
// without kinds
x : forall { a, b, n }
  => (a -> b) [n] // function used `n` times
  -> vec(a, n) // vec `a` with length `n`
  -> vec(b, n) // vec `b` with length `n`

id : forall {a : *} => a -> a
id = lambda (x) => x

//
// Holes

// type operators?
data a || b =
  | In1 of a
  | In2 of b
data a && b =
  | And of a * b

in1 : forall {a, b} => a -> a || b = _
in2 : forall {a, b} => b -> a || b = _

swap : forall {a, b} => a || b -> b || a = _
elim : forall {a, b, r} => (a -> r) -> (b -> r) -> a || b -> r = _

//

data Option(a) =
  | None
  | Some(a)

some : forall {a} => Option(a)
some = lambda (x) => Some(x)

none : forall {a} => Option(a)
none = None

//

data Nat =
  | Z
  | S(Nat)

fold : forall {a} -> Nat -> a -> (Nat -> a) -> a
fold = lambda (num, z, s) =>
  case num
  | Z -> z
  | S(k) -> s k

pred : Nat -> Option(Nat)
pred = lambda num => fold(num, None, Some)

to_int : Nat -> Int
to_int = lambda num => fold(num, 0, lambda x => to_int(x - 1))

//

data Either(a, b) =
  | Left(a)
  | Right(b)

swap : forall {a, b} -> Either(a, b) -> Either(b, a) =>
swap = lambda either =>
  case either
  | Right(x) => Left(x)
  | Left(x) => Right(x)

type Maybe(a) =
  | Some(a)
  | None

bind : forall {a} => Maybe(a)
bind = lambda val => {
  Some(content)
}

bind : forall {a} => Maybe(a)
bind = lambda val => Some(val)

bind = val => {
  Some(val)
}

bind = val => Some(val)

import stdlib/ppx/language (parser, format)

show = (expr) => {
  case (parser(expr))
  | Pexp_constructor (tag, Some (value)) => format("%s(%s)", (tag, show(parser)))
  | Pexp_constructor (tag, None) => format("%s", tag)
  | _ => format("Not implemented")
}

type Char = Int
type String = List<Char>

type Term(a) =
  | Var(Int)
  | Abs(Term)
  | App(Term, Term)

line = 10
null = 0

// rec flag?
rec flatten = lambda xs =>
  case xs
  | hd :: tl => concat(hd, (flatten(tl)))
  | [] => []

range : Int -> List(Int) -> List(Int)
range = lambda (n, acc) =>
  case n
  | 0 => acc
  | _ => range(n - 1, n :: acc)

concat : forall {a} => List(a) -> List(a) -> List(a)
concat = (xs, ys) => {
  case (xs, ys)
  | ([], ys) =>
  | (hd :: tl, ys) => hd :: (concat(tl, ys))
}

zip : forall {a, b} => List(a) -> List(b) -> (a * b) list
zip xs ys =
  case (xs, ys)
  | (h1 :: t1, h2 :: t2) => (h1, h2) :: zip t1 t2
  | (_, _) => []

zip_with : forall {a, b, c} -> List(a) -> List(b) -> (a -> b -> c) -> List(c)
zip_with = lambda (xs, ys, f) => {
  case (xs, ys)
  | (h1 :: t1, h2 :: t2) => f(h1, h2) :: zip_with(t1, t2, f)
  | (_, _) => []
}

mut : forall {a} => List(a) -> (a -> a) -> Int -> List(a)
mut xs f index =
  case (xs, index)
  | (hd :: tl), 0 => f(hd) :: tl
  | (hd :: tl), n => hd :: mut(tl, f, (n - 1))
  | ([], n) => []

(==) : forall {a} => a -> a -> Bool
(==) = lambda (a, b) => equal(a, b)

(!=) : forall {a} => a -> a -> Bool
(!=) = lambda (a, b) => not(equal(a, b))

('a' == 'a')
(1 != 0)
(a == a)

(+) : int -> int -> int = (a, b) => plus(a, b)
(+) = (a, b) => plus(a, b)

(<|) : forall {a, b} => a -> (a -> b) -> b
(<|) = lambda (x, f) => f(x)

(|>) : forall {a, b} => (a -> b) -> a -> b
(|>) = lambda (f, x) => f(x)

apply =
  x + 1
  |> ((-) 1)
  |> ((+) 2)
  |> ((*) 3)

// modules, goals:
// - recursive module definitions
// - structure and signature inheritance (open, include)
// - signature ascription (annotations), opaque (:>) and transparent (:)
// - functor application

User : {
  // module signature / context
  type t

  make : string -> number -> t
} = {
  type t = (string * number)

  make name age = (name, age)
}

Name : {
  type t

  make : t -> t
  empty : t
  equal : t -> t -> boolean
} = {
  type t = string

  make t = t
  empty = String.empty
  equal = String.equal
}

Eq : {
  type t;
  eq : t -> t -> bool
}

Int = {
  type t = Int

  between : t -> t -> t -> Bool
  between = lambda (l, value, r) => (l <= value) && (value < r)
}

Char = {
  type t = Char

  between : t -> t -> t -> Bool
  between l, value, r =
    Int.between(l, value, r)

  repeat : t -> Int -> String
  repeat c n =
    case (n)
    | 0 => String.Nil
    | x => String.Cons(c, repeat(c, x - 1))
}

Option = {
  type Option(a) =
    | None
    | Some(a)

  map : forall {a, b} => (a -> b) -> a -> b
  map = lambda (f, m) =>
    case m
    | Some(x) => Some(f(x))
    | None => None

  (>>=) : forall {a, b} => (a -> b) -> a -> b
  (>>=) = lambda(f, m) => map(f, m)
}

// ?
_ = Option.map (lambda (x) => x + 5) Option.None
_ = Option.map (lambda (x) => x + 5) Option.Some (5)

// GADT
data Box =
  | Box : forall {a} => ((a -> String) * a) -> Box

data Box =
  | Box : ((a -> String) * a) -> Box

show = lambda (Box(f, x)) => f(x)
```
