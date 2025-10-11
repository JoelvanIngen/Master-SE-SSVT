------------------------ MODULE bc_group6 ------------------------
EXTENDS Integers, Naturals, Sequences \* The necessary "imports" to run the algorithm
CONSTANT capacity, bound \* The algorithm's constants - the island's capacity and max number events


(*
--algorithm bridgeController {
    variable n = 0, i = 0;

    procedure enter_island(){
        enter: n := n + 1;
        return;
    }

    procedure leave_island(){
        leave: n := n - 1;
        return;
    }

    {
        main: while (i < bound) {
            either
                if (n < capacity){
                    call enter_island();
                }
            or
                call leave_island();

            progress: i := i + 1;
        }
    }
}
*)

\* BEGIN TRANSLATION (chksum(pcal) = "1ffe53d7" /\ chksum(tla) = "43163dd")
VARIABLES pc, n, i, stack

vars == << pc, n, i, stack >>

Init == (* Global variables *)
        /\ n = 0
        /\ i = 0
        /\ stack = << >>
        /\ pc = "main"

enter == /\ pc = "enter"
         /\ n' = n + 1
         /\ pc' = Head(stack).pc
         /\ stack' = Tail(stack)
         /\ i' = i

enter_island == enter

leave == /\ pc = "leave"
         /\ n' = n - 1
         /\ pc' = Head(stack).pc
         /\ stack' = Tail(stack)
         /\ i' = i

leave_island == leave

main == /\ pc = "main"
        /\ IF i < bound
              THEN /\ \/ /\ IF n < capacity
                               THEN /\ stack' = << [ procedure |->  "enter_island",
                                                     pc        |->  "progress" ] >>
                                                 \o stack
                                    /\ pc' = "enter"
                               ELSE /\ pc' = "progress"
                                    /\ stack' = stack
                      \/ /\ stack' = << [ procedure |->  "leave_island",
                                          pc        |->  "progress" ] >>
                                      \o stack
                         /\ pc' = "leave"
              ELSE /\ pc' = "Done"
                   /\ stack' = stack
        /\ UNCHANGED << n, i >>

progress == /\ pc = "progress"
            /\ i' = i + 1
            /\ pc' = "main"
            /\ UNCHANGED << n, stack >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == enter_island \/ leave_island \/ main \/ progress
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 


\* Boolean properties for model checking invariants
inv0 == n <= capacity \* the maximum number of visitors can't exceed the capacity
inv1 == i <= bound

(*
I used a capacity of 50 and a bound of 1000.
The capacity was chosen to be 50 in order to be able to do different combinations of leaving and entering.
The bound was chosen to be much higher than the capacity in order to reach every possible state.
*)


=============================================================================
\* Modification History
\* Last modified Thu Oct 09 13:56:45 CEST 2025 by jasperwink
\* Last modified Wed Aug 13 16:02:24 CEST 2025 by tv
\* Created Mon Jun 16 14:24:22 CEST 2025 by tv
