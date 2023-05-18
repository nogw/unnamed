- I need:
  - Forall syntax
- I want:
  - Bidirectional type inference
  - Linear types
  - Algebraic datatypes (ADTs)
  - Pattern matching
  - Exhaustiveness checking
  - Typeclasses (including multi-parameter typeclasses)
  - Higher-kinded types
  - Higher-rank polymorphism
  - Laziness
  - Syntax for infix operators
  - Scoped type variables

open Typer

type test = { received : string; expected : string }

let make_test ~received ~expected = { received; expected }

let test1 = make_test
~received:"lambda x => x"
~expected:"forall [a] => a -> a"

let test2 = make_test
~received:"lambda x => lambda y => y"
~expected:"forall [a b] => a -> b -> b"

let test3 = make_test
~received:"lambda x => lambda y => x"
~expected:"forall [a b] => a -> b -> a"

let test4 = make_test
~received:"let x = lambda y => y; x"
~expected:"forall [a] => a -> a"

let test5 = make_test
~received:"lambda x => let y = lambda z => z; y"
~expected:"forall [a b] => a -> b -> b"

let test6 = make_test
~received:"lambda x => x(lambda y => y)"
~expected:"forall [a b] => ((a -> a) -> b) -> b"

let test7 = make_test
~received:"lambda x => let y = let z = x(lambda x => x); z; y"
~expected:"forall [a b] => ((a -> a) -> b) -> b"

let test8 = make_test
~received:"lambda x => let y = lambda z => x; y"
~expected:"forall [a b] => a -> b -> a"

let parse_type pretype =
let pretype = Lexer.from_string Parser.pretype pretype in
let pretype = Option.get pretype in
pretype

let parse_expr expr =
let expr = Lexer.from_string Parser.term expr in
let expr = Option.get expr in
expr

let test_type { received; expected } =
let check () =
let core = Environment.empty in
let received_type =
let received = parse_expr received in
let received = Transl.transl_expr ~env:core received in
let received = infer ~env:core 0 received in
received
in
let expected_type =
let expected = parse_type expected in
let expected = Transl.transl_wrapper core expected in
snd expected
in
let type_equal = Alcotest.testable TTree.pp_poly TTree.equal_poly in
Alcotest.((check' type_equal) ~msg:"same type" ~actual:received_type ~expected:expected_type)
in
Alcotest.test_case received `Quick check

let tests = [
test1;
test2;
test3;
test4;
test5;
test6;
test7;
test8;
]

let tests = ("Test infer", List.map test_type tests)

let test () = Alcotest.run "Infer" [ tests ]
