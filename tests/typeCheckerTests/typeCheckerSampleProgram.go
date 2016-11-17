(* Sample program on the project website - valid and passes type checking *)

{
  x := 1;
  y := true;
  ch := newChannel;

  go {  while y {
           ch <- 1
        }
     };

  while y {
    <-ch
  };
 x = x + 1
}
