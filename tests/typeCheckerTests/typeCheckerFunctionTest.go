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

func testbooleanmultiple(a int, b bool, cd chan int) bool {   (* mul letter, return bool, mul arg *)
  a = a + 1;
  b = !b;
  c := <- cd;
  return b
}

func testchannel() chan int {
  a := newChannel;
  return a
}

func testchannelmul(a chan int) chan int {  (* mul letter, return chan int, 1 arg *)
  return a
}

func sometestforfunccallfuncexp() int {
  return 0
}

func twoparams(a int, b int) int {
  return 0
}

{
  sometestforfunccallfuncexp();
  x := sometestforfunccallfuncexp();
  twoparams(1, 2);
  x := twoparams(1, 2);
  return 0
}
