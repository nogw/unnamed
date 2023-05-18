open Typer

type test = { received : string; expected : string }

let make_test ~received ~expected = { received; expected }

let test1 = 
  make_test 
    ~received:"lambda x => x" 
    ~expected:"forall [a] => a -> a"

let test2 =
  make_test
    ~received:"lambda x => lambda y => y"
    ~expected:"forall [a, b] => a -> b -> b"

let test3 =
  make_test
    ~received:"lambda x => lambda y => x"
    ~expected:"forall [a, b] => a -> b -> a"

let test4 =
  make_test
    ~received:"let x = lambda y => y; x"
    ~expected:"forall [a] => a -> a"

let test5 =
  make_test
    ~received:"lambda x => let y = lambda z => z; y"
    ~expected:"forall [a, b] => a -> b -> b"

let test6 =
  make_test
    ~received:"lambda x => x((lambda y => y))"
    ~expected:"forall [a, b] => ((a -> a) -> b) -> b"

let test7 =
  make_test
    ~received:"lambda x => let y = let z = x((lambda x => x)); z; y"
    ~expected:"forall [a, b] => ((a -> a) -> b) -> b"

let test8 =
  make_test
    ~received:"lambda x => let y = lambda z => x; y"
    ~expected:"forall [a, b] => a -> b -> a"

let test9 =
  make_test
    ~received:"lambda f => lambda x => f(x)"
    ~expected:"forall [a, b] => (a -> b) -> a -> b"

let test10 =
  make_test
    ~received:"lambda x => let y = lambda z => x(z); y"
    ~expected:"forall [a, b] => (a -> b) -> a -> b"

let test11 =
  make_test
    ~received:"lambda x => lambda y => let x = x(y); lambda x => y(x)"
    ~expected:"forall [a, b, c] => ((a -> b) -> c) -> (a -> b) -> a -> b"

let test12 =
  make_test
    ~received:"lambda f => lambda a => lambda b => f(b, a)"
    ~expected:"forall [a, b, c] => (a -> b -> c) -> b -> a -> c"

let parse_type pretype =
  let pretype = Lexer.from_string Parser.type_opt pretype in
  let pretype = Option.get pretype in
  pretype

let parse_expr expr =
  let expr = Lexer.from_string Parser.expr_opt expr in
  let expr = Option.get expr in
  expr

let test_type { received; expected } =
  let check () =
    let actual =
      Skolem.Fresh.reset () ;
      let core = Core.initial_env in
      let received = parse_expr received in
      let received = Transl.transl_expr ~env:core received in
      let received = infer ~env:core 0 received in
      Show.of_type received
    in
    let expected =
      Skolem.Fresh.reset () ;
      let core = Core.initial_env in
      let expected = parse_type expected in
      let expected = Transl.transl_wrapper core expected in
      Show.of_type (snd expected)
    in
    Alcotest.((check string) "same type" actual expected)
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
  ]

let tests = "Test infer", List.map test_type tests
let test () = Alcotest.run "Infer" [ tests ]
