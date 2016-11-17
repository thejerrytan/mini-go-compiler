(* Fails - incompatible types *)

{
  x := newChannel;
  z := <-x;
  z = z && true
}
