(* A simple stack-based VM with shared memory *)

(* VM supports only integers, so booleans need to be mapped to integer
   where 0 = false and 1 = true

Each thread has two type of stacks.
One for arithmetic computations and one for maintaining run-time information.

There are no registers but we can make use of some global memory cells.

There is also support for concurrency by starting a new thread
and locking memory cells.

 *)

type instructions =
                  Halt
                  | Skip
                  (* Stack operations 
 
                      PopS simply drops top-most value
                      Add  pops two top-most values and
                           pushes result onto stack
                          
                           The order matters for Div, ...
                           We first pop the left then the right operand

                      Output Prints top-most value onto console

                  *)
                  | PushS of int
                  | PopS
                  | Add 
                  | Sub
                  | Div
                  | Mult
                  | Lt
                  | Gt
                  | Eq

                  | Output

                  (* (Conditional) jumps 

                     Set PC to position indicated where
                     the position refers to the position in the list of instructions.
                     First position = 0 !

                     Jump = absolute address

                     JumpMemLoc == address stored in memory location,
                                   necessary for "return"

                     NonZero/Zero will pop the jump address.

                  *)
                  | NonZero of int
                  | Zero of int
                  | Jump of int
                  | JumpMemLoc of int                              
          

                  (* Memory operations 

                    Assign (memLoc, valInteger)

                    PushToStack memLoc

                           push value stored in memory cell memLoc onto stack

                    AssignFromStack (relStackPos, memLoc)
                        stack remains unchanged
                        relStackPos = 1 to access top-most element

                  *)

                  | Assign of int*int
                  | PushToStack of int
                  | AssignFromStack of int*int

                  (* run-time (environment) stack 

                      PushE i
                      PopE
 
                          as for computation stack

                      PushToEnv memLoc                 
 
                           push value stored in memory cell memLoc onto env stack

                      AssignFromEnv (relEnvStackPos, memLoc)

                        same as AssignFromStack but for env stack


                      UpdateToEnv (relEnvStackPos, memLoc)

                         Update relEnvStackPos with memLoc

                         This is more of a convenience function as we could
 
                         1. pop the top-most relEnvStack positions and store
                            them into some temporary memory locations

                         2. pop the top-most (the position we are interested in)

                         3. push memLoc (its value)

                         4. restore the other elements by pushing their values
                            stored into some temporary memory locations
                            back onto the stack

                  *)

                  | PushE of int
                  | PopE
                  | PushToEnv of int
                  | AssignFromEnv of int*int
                  | UpdateToEnv of int*int                                           

                  (* shared memory and concurrency *)
                  | Lock of int
                  | Unlock of int
                  | Thread of instructions list
[@@deriving show]

type lockInfo = { locked : bool;
                  threadID : int }                             

(* sp and ep refer to the next available position *)
type thread = { pc : int ref;
                code : instructions list;
                stack : int array;
                env : int array;
                sp : int ref;
                ep : int ref}

type state = { mem : int array;
               memLock : lockInfo array;
               threads : (thread list) ref;
               activeThread : int ref}
               

let nameSupply = ref 1
let fresh _ =  nameSupply := !nameSupply + 1;
               !nameSupply

(* global shared memory *)
let memSize = 20000
let mkMem _ = Array.make memSize 0

let mkMemLock _ = Array.make memSize {locked = false; threadID = 0}
                         
(* computation stack *)
let stkSize = 20000
let mkStk _ = Array.make stkSize 0

(* run-time environment stack *)
let envSize = 20000
let mkEnv _ = Array.make envSize 0
               
let mkThread cs = { pc = ref 0;
                    code = cs;
                    stack = mkStk();
                    env = mkEnv();
                    sp = ref 0;
                    ep = ref 0 }

let initState cs = { mem = mkMem();
                     memLock = mkMemLock();
                     threads = ref [mkThread cs];
                     activeThread = ref 0}

let inc r = r := !r + 1
let dec r = r := !r - 1 
                     
                    
let singleStep st id mem memLock t = match (List.nth t.code !(t.pc)) with
  | Halt ->   true
  | Skip -> inc t.pc;
            false
  | PushS i -> t.stack.(!(t.sp)) <- i;
               inc t.pc;
               inc t.sp;
               false

  | PopS -> inc t.pc;
            dec t.sp;
            false            

  (* recall that sp refers to the next available position,
     so must subtract 1 to access top element *)              
  | Add -> let i = !(t.sp) - 1
           in t.stack.(i-1) <- t.stack.(i-1) + t.stack.(i);
              inc t.pc;
              dec t.sp;
              false

  | Sub -> let i = !(t.sp) - 1
           in t.stack.(i-1) <- t.stack.(i-1) - t.stack.(i);
              inc t.pc;
              dec t.sp;
              false

  | Div -> let i = !(t.sp) - 1
           in t.stack.(i-1) <- t.stack.(i-1) / t.stack.(i);
              inc t.pc;
              dec t.sp;
              false

  | Mult -> let i = !(t.sp) - 1
            in t.stack.(i-1) <- t.stack.(i-1) * t.stack.(i);
               inc t.pc;
               dec t.sp;
               false                                

  | Lt -> let i = !(t.sp) - 1
          in (if t.stack.(i-1) < t.stack.(i)
              then t.stack.(i-1) <- 1
              else t.stack.(i-1) <- 0);
             inc t.pc;
             dec t.sp;
             false

  | Gt -> let i = !(t.sp) - 1
          in (if t.stack.(i-1) > t.stack.(i)
              then t.stack.(i-1) <- 1
              else t.stack.(i-1) <- 0);
             inc t.pc;
             dec t.sp;
             false

  | Eq -> let i = !(t.sp) - 1
          in (if t.stack.(i-1) == t.stack.(i)
              then t.stack.(i-1) <- 1
              else t.stack.(i-1) <- 0);
             inc t.pc;
             dec t.sp;
             false                              
                 
  | Output -> Printf.printf "%d \n" t.stack.(!(t.sp) - 1);
              inc t.pc;
              false

  | NonZero i -> let x = t.stack.(!(t.sp) - 1) in
                 dec t.sp;
                 (if x == 0
                 then inc t.pc
                 else t.pc := i);
                 false

  | Zero i ->    let x = t.stack.(!(t.sp) - 1) in
                 dec t.sp;
                 (if x == 0
                 then t.pc := i
                 else inc t.pc);
                 false

  | Jump i -> t.pc := i;
              false

  | JumpMemLoc loc -> t.pc := mem.(loc);
                      false                
 
  | Assign (loc,i) -> inc t.pc;
                      mem.(loc) <- i;
                      false

  | PushToStack loc -> inc t.pc;
                       t.stack.(!(t.sp)) <- mem.(loc);
                       inc t.sp;
                       false

  (* deref of the relative position relPos and assign to mem loc,
     to access top-most stack element set relPos=1,
     recall that sp refers to the next available position *)                         
  | AssignFromStack (relPos,loc) -> inc t.pc;
                                    mem.(loc) <- t.stack.(!(t.sp) - relPos);
                                    false

  (* lock a memory cell, note that acess via assign doesn't check if cell is locked,
     hence, we assume that in case of shared memory, every access is protected by lock *)                                      
  | Lock loc -> if memLock.(loc).locked
                then true
                else (memLock.(loc) <- {locked = true; threadID = id};
                      inc t.pc;
                      false)

  (* only the owner can unlock a memory cell, multiple unlock yield failure (true) *)                       
  | Unlock loc -> if (not (memLock.(loc).locked && memLock.(loc).threadID == id))
                  then true
                  else (memLock.(loc) <- {locked = false; threadID = id};
                      inc t.pc;
                      false)
                         
  (* add new thread *)
  | Thread cs -> let ts = !(st.threads) in
                 st.threads := List.append ts [mkThread cs];
                 false                  
                         
  | PushE i -> t.env.(!(t.ep)) <- i;
               inc t.pc;
               inc t.ep;
               false

  | PopE -> inc t.pc;
            dec t.ep;
            false

  | PushToEnv loc -> inc t.pc;
                     t.env.(!(t.ep)) <- mem.(loc);
                     inc t.ep;
                     false

  | AssignFromEnv (relPos,loc) -> inc t.pc;
                                  mem.(loc) <- t.env.(!(t.ep) - relPos);
                                  false

  | UpdateToEnv (relPos,loc) -> inc t.pc;
                                t.env.(!(t.ep) - relPos) <- mem.(loc);
                                false                                          
                       
let debug txt = Printf.printf txt;
                Printf.printf "\n"
                
let run cs = let st = initState cs in
             let stop = ref false in
             while not !stop do
                  stop := true;
                  for i = 0 to List.length !(st.threads) - 1 do 
                    if not (singleStep st i st.mem st.memLock (List.nth !(st.threads) i))
                    then stop := false
                  done;
             done;
             st
                        
let testProg1 = [PushS 1; PushS 2; Add; Output; Halt]

let testProg2 = [PushS 1; PushS 2; Lt; Output; Halt]                  
                          
let getThread st i = (List.nth !(st.threads) i)

let _ = run testProg2;;