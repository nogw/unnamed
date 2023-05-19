open Typer

type test = { received : string; expected : string }

let make_test ~received ~expected = { received; expected }

let test1 = 
  make_test 
    ~received:"lambda x => x" 
    ~expected:"forall {a} => a -> a"

let test2 =
  make_test
    ~received:"lambda x => lambda y => y"
    ~expected:"forall {a, b} => a -> b -> b"

let test3 =
  make_test
    ~received:"lambda x => lambda y => x"
    ~expected:"forall {a, b} => a -> b -> a"

let test4 =
  make_test
    ~received:"let x = lambda y => y; x"
    ~expected:"forall {a} => a -> a"

let test5 =
  make_test
    ~received:"lambda x => let y = lambda z => z; y"
    ~expected:"forall {a, b} => a -> b -> b"

let test6 =
  make_test
    ~received:"lambda x => x((lambda y => y))"
    ~expected:"forall {a, b} => ((a -> a) -> b) -> b"

let test7 =
  make_test
    ~received:"lambda x => let y = let z = x((lambda x => x)); z; y"
    ~expected:"forall {a, b} => ((a -> a) -> b) -> b"

let test8 =
  make_test
    ~received:"lambda x => let y = lambda z => x; y"
    ~expected:"forall {a, b} => a -> b -> a"

let test9 =
  make_test
    ~received:"lambda f => lambda x => f(x)"
    ~expected:"forall {a, b} => (a -> b) -> a -> b"

let test10 =
  make_test
    ~received:"lambda x => let y = lambda z => x(z); y"
    ~expected:"forall {a, b} => (a -> b) -> a -> b"

let test11 =
  make_test
    ~received:"lambda x => lambda y => let x = x(y); lambda x => y(x)"
    ~expected:"forall {a, b, c} => ((a -> b) -> c) -> (a -> b) -> a -> b"

let test12 =
  make_test
    ~received:"lambda f => lambda a => lambda b => f(b, a)"
    ~expected:"forall {a, b, c} => (a -> b -> c) -> b -> a -> c"

let test13 =
  make_test
    ~received:"let id = lambda x => x; (id(10), id(()))"
    ~expected:"(Int * Unit)"

let test14 =
  make_test
    ~received:"let f = lambda x => x; f((lambda x => x), (lambda _ => 10))"
    ~expected:"forall {a} => a -> Int"

let test15 =
  make_test
    ~received:"let f = lambda x => x; (f((lambda x => x)), f((lambda _ => 10)))"
    ~expected:"((forall {a} => a -> a) * (forall {a} => a -> Int))"

let test_type { received; expected } =
  let process_expr expr =
    Skolem.Fresh.reset ();
    let core = Core.initial_env in
    let expr = Lexer.from_string Parser.expr_opt expr in
    let expr = Option.get expr in
    let expr = Transl.transl_expr ~env:core expr in
    let expr = infer ~env:core 0 expr in
    Show.of_type expr
  in
  let process_type ty =
    Skolem.Fresh.reset ();
    let core = Core.initial_env in
    let () = Format.printf "(%s)\n" ty in
    let ty = Lexer.from_string Parser.type_opt ty in
    let ty = Option.get ty in
    let () = Format.printf "(%a)\n" Printer.pp_type ty in
    let ty = Transl.transl_wrapper core ty in
    Show.of_type (snd ty)
  in
  let check () =
    let actual = process_expr received in
    let expected = process_type expected in
    Alcotest.(check string) "same type" actual expected
  in
  Alcotest.test_case received `Quick check

let tests =
  [
    test1;
    test2;
    test3;
    test4;
    test5;
    test6;
    test7;
    test8;
    test9;
    test10;
    test11;
    test12;
    test13;
    test14;
    test15;
  ]

let tests = "Test infer", List.map test_type tests
let test () = Alcotest.run "Infer" [ tests ]
