open Intermediate
open Vm


let codegen irc : (Vm.instructions list option) = 
  Some [Vm.Halt; Vm.PopE]
