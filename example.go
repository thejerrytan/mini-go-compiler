func s(a int, b int) int {
	return 1
}

func sum(a int, b int) int {
	return a + b
}

{ 
  a := 1 + 2 + sum(12, 777);
  return a
}