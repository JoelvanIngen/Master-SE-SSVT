------------------------ MODULE bc_6 ------------------------
EXTENDS Integers, Naturals, Sequences \* The necessary "imports" to run the algorithm
CONSTANT capacity \* The algorithm's constants - the island's capacity and max number events


(*
--algorithm bridgeController {
    variable n = 0;

    procedure enter_island(){
        enter: n := n + 1;
        return;
    }

    procedure leave_island(){
        leave: n := n - 1;
        return;
    }

    {
        main: while (TRUE) {
            either
                if (n < capacity){
                    call enter_island();
                }
            or
                if (n > 0) {
                    call leave_island();
                }
        }
    }
}
*)

\* BEGIN TRANSLATION (chksum(pcal) = "7b4dbe98" /\ chksum(tla) = "bc4c4fc")
VARIABLES pc, n, stack

vars == << pc, n, stack >>

Init == (* Global variables *)
        /\ n = 0
        /\ stack = << >>
        /\ pc = "main"

enter == /\ pc = "enter"
         /\ n' = n + 1
         /\ pc' = Head(stack).pc
         /\ stack' = Tail(stack)

enter_island == enter

leave == /\ pc = "leave"
         /\ n' = n - 1
         /\ pc' = Head(stack).pc
         /\ stack' = Tail(stack)

leave_island == leave

main == /\ pc = "main"
        /\ \/ /\ IF n < capacity
                    THEN /\ stack' = << [ procedure |->  "enter_island",
                                          pc        |->  "main" ] >>
                                      \o stack
                         /\ pc' = "enter"
                    ELSE /\ pc' = "main"
                         /\ stack' = stack
           \/ /\ IF n > 0
                    THEN /\ stack' = << [ procedure |->  "leave_island",
                                          pc        |->  "main" ] >>
                                      \o stack
                         /\ pc' = "leave"
                    ELSE /\ pc' = "main"
                         /\ stack' = stack
        /\ n' = n

Next == enter_island \/ leave_island \/ main

Spec == Init /\ [][Next]_vars

\* END TRANSLATION 


\* Boolean properties for model checking invariants
inv0 == n <= capacity \* the maximum number of visitors can't exceed the capacity.
inv1 == n >= 0 \* The number of people on the island cannot be negative.

(*
A important design decision I made was to remove the bound.
This was replaced by the while(TRUE), and this allows the model to simulate all scenarions and not just the ones it happens to find while in those bounds.

The invariants we used are that n is not bigger than the capacity in order to not overrun the island.
Beside that we had an invariant that n cannot be smaller than 0 because that is not a scenario that can happen in real life.
The model was tested with a capacity of 1000, 10000 and 100000 in order to verify the model.
No major differences between the capacity sizes were found besides the increase in distinct state count which is to be expected.
*)


=============================================================================
\* Modification History
\* Last modified Tue Oct 14 13:20:56 CEST 2025 by jasperwink
\* Last modified Wed Aug 13 16:02:24 CEST 2025 by tv
\* Created Mon Jun 16 14:24:22 CEST 2025 by tv
