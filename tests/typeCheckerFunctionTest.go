func s() int {  (* single letter, return int, no arg *)
  return 0
}

func s() {  (* single letter, return void, no arg *)
  a := 1
}

func sum(a int, b int, c int) {  (* mul letter, return void, 3 arg *)
  d := a + b + c
}

func inc(a int) int {  (* mul letter, return int, 1 arg *)
  return a + 1
}

func conditionalinc(a int, b bool) {  (* mul letter, return void, 2 arg *)
  if (b) {
    a = a + 1
  } else {
    a = a
  }
}

func testboolean() bool {  (* mul letter, return bool, no arg *)
  return true
}

(* TODO: testBooleanMultiple is failing *)

(* TODO: testChannel is failing *)

func testchannelmul(a chan int) chan int {  (* mul letter, return chan int, 1 arg *)
  return a
}

{
  return 0
}
