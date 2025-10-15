----------------------- MODULE dd_6 ------------------------
EXTENDS Integers, Naturals, Sequences \* The necessary "imports" to run the algorithm
\* NB: this code contains *much* fewer labels than the bridge_controller model. This allows you to add labels where you find them most informative.
(*
--algorithm double_door {
    \* Initialisation.
    variable left_door = "closed"; 
             right_door = "closed";
             left = "empty";
             mid = "empty";
             right = "empty";

             left_door_opened_from = "none";
             right_door_opened_from = "none";
             last_door_opened_from = "none";

             unauthorized_mid_entry = FALSE;
             unauthorized_mid_exit = FALSE;


    \* Someone may arrive at the left or right side of the doors.
    procedure new_arrival(){
        arrive:
        either
            if (left = "empty") left := "left_person";
        or
            if (right = "empty") right := "right_person";
        return;   
    }

    \* If a person can leave if it is past both doors or after arriving.
    procedure person_leaves(){
        depart:
        either
            if (left # "empty") left := "empty";
        or
            if (right # "empty") right := "empty";
        departed: return;
    }

    \* Someone opens a door. Note that we assume people coming from the left only move rightwards and vice-versa.
    procedure open_door(){
        check_openable: 
        if (left_door = "closed" /\ right_door = "closed") {
            open:
            either
                if (left = "left_person" /\ ~(last_door_opened_from = "inside" /\ mid # "empty")) {
                    left_door := "open";
                    left_door_opened_from := "outside";
                    last_door_opened_from := "outside";
                };
            or
                if (mid = "right_person") {
                    left_door := "open";
                    left_door_opened_from := "inside";
                    last_door_opened_from := "inside";
                };
            or
                if (right = "right_person" /\ ~(last_door_opened_from = "inside" /\ mid # "empty")) {
                    right_door := "open";
                    right_door_opened_from := "outside";
                    last_door_opened_from := "outside";
                };
            or 
                if (mid = "left_person") {
                    right_door := "open";
                    right_door_opened_from := "inside";
                    last_door_opened_from := "inside";
                };
        };
        opened: return;
    }

    \* The door closes if a door is open.
    procedure close_door(){
        close:
        either
            if (left_door = "open") left_door := "closed";
        or
            if (right_door = "open") right_door := "closed";
        closed: return;
    }

    \* Someone walks through a door. People always exit before others enter. 
    \* Note that this procedure presumes both doors are never open at the same time!
    procedure walk(){
        who_walks:
        if (mid = "left_person" /\ right_door = "open") {
                    right := mid;
                    mid := "empty";
                    if (right_door_opened_from = "outside")
                        unauthorized_mid_exit := TRUE;
                    else
                        unauthorized_mid_exit := FALSE;
        };
        else if (mid = "right_person" /\ left_door = "open") {
                    left := mid;
                    mid := "empty";
                    if (left_door_opened_from = "outside")
                        unauthorized_mid_exit := TRUE;
                    else
                        unauthorized_mid_exit := FALSE;
        };
        else if (mid = "empty") {
            if (left = "left_person" /\ left_door = "open") {
                    mid := left;
                    left := "empty";
                    if (left_door_opened_from = "inside")
                        unauthorized_mid_entry := TRUE;
                    else 
                        unauthorized_mid_entry := FALSE;
            };
            else if (right = "right_person" /\ right_door = "open") {
                    mid := right;
                    right := "empty";
                    if (right_door_opened_from = "inside")
                        unauthorized_mid_entry := TRUE;
                    else
                        unauthorized_mid_entry := FALSE;
            };
        };
        return;
    }

    {
    \* Nondeterministically choose an action. Note that this will not loop forever - TLA+ will find all possible states the loop generates and then terminate.   
    main: while (TRUE) {
        either
            call new_arrival();
        or
            call person_leaves();
        or
            call open_door();
        or
            call close_door();
        or
            call walk();
    }
    }
}
*)
\* BEGIN TRANSLATION (chksum(pcal) = "ede455ab" /\ chksum(tla) = "a378c923")
VARIABLES pc, left_door, right_door, left, mid, right, left_door_opened_from, 
          right_door_opened_from, last_door_opened_from, 
          unauthorized_mid_entry, unauthorized_mid_exit, stack

vars == << pc, left_door, right_door, left, mid, right, left_door_opened_from, 
           right_door_opened_from, last_door_opened_from, 
           unauthorized_mid_entry, unauthorized_mid_exit, stack >>

Init == (* Global variables *)
        /\ left_door = "closed"
        /\ right_door = "closed"
        /\ left = "empty"
        /\ mid = "empty"
        /\ right = "empty"
        /\ left_door_opened_from = "none"
        /\ right_door_opened_from = "none"
        /\ last_door_opened_from = "none"
        /\ unauthorized_mid_entry = FALSE
        /\ unauthorized_mid_exit = FALSE
        /\ stack = << >>
        /\ pc = "main"

arrive == /\ pc = "arrive"
          /\ \/ /\ IF left = "empty"
                      THEN /\ left' = "left_person"
                      ELSE /\ TRUE
                           /\ left' = left
                /\ right' = right
             \/ /\ IF right = "empty"
                      THEN /\ right' = "right_person"
                      ELSE /\ TRUE
                           /\ right' = right
                /\ left' = left
          /\ pc' = Head(stack).pc
          /\ stack' = Tail(stack)
          /\ UNCHANGED << left_door, right_door, mid, left_door_opened_from, 
                          right_door_opened_from, last_door_opened_from, 
                          unauthorized_mid_entry, unauthorized_mid_exit >>

new_arrival == arrive

depart == /\ pc = "depart"
          /\ \/ /\ IF left # "empty"
                      THEN /\ left' = "empty"
                      ELSE /\ TRUE
                           /\ left' = left
                /\ right' = right
             \/ /\ IF right # "empty"
                      THEN /\ right' = "empty"
                      ELSE /\ TRUE
                           /\ right' = right
                /\ left' = left
          /\ pc' = "departed"
          /\ UNCHANGED << left_door, right_door, mid, left_door_opened_from, 
                          right_door_opened_from, last_door_opened_from, 
                          unauthorized_mid_entry, unauthorized_mid_exit, stack >>

departed == /\ pc = "departed"
            /\ pc' = Head(stack).pc
            /\ stack' = Tail(stack)
            /\ UNCHANGED << left_door, right_door, left, mid, right, 
                            left_door_opened_from, right_door_opened_from, 
                            last_door_opened_from, unauthorized_mid_entry, 
                            unauthorized_mid_exit >>

person_leaves == depart \/ departed

check_openable == /\ pc = "check_openable"
                  /\ IF left_door = "closed" /\ right_door = "closed"
                        THEN /\ pc' = "open"
                        ELSE /\ pc' = "opened"
                  /\ UNCHANGED << left_door, right_door, left, mid, right, 
                                  left_door_opened_from, 
                                  right_door_opened_from, 
                                  last_door_opened_from, 
                                  unauthorized_mid_entry, 
                                  unauthorized_mid_exit, stack >>

open == /\ pc = "open"
        /\ \/ /\ IF left = "left_person" /\ ~(last_door_opened_from = "inside" /\ mid # "empty")
                    THEN /\ left_door' = "open"
                         /\ left_door_opened_from' = "outside"
                         /\ last_door_opened_from' = "outside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << left_door, left_door_opened_from, 
                                         last_door_opened_from >>
              /\ UNCHANGED <<right_door, right_door_opened_from>>
           \/ /\ IF mid = "right_person"
                    THEN /\ left_door' = "open"
                         /\ left_door_opened_from' = "inside"
                         /\ last_door_opened_from' = "inside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << left_door, left_door_opened_from, 
                                         last_door_opened_from >>
              /\ UNCHANGED <<right_door, right_door_opened_from>>
           \/ /\ IF right = "right_person" /\ ~(last_door_opened_from = "inside" /\ mid # "empty")
                    THEN /\ right_door' = "open"
                         /\ right_door_opened_from' = "outside"
                         /\ last_door_opened_from' = "outside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << right_door, right_door_opened_from, 
                                         last_door_opened_from >>
              /\ UNCHANGED <<left_door, left_door_opened_from>>
           \/ /\ IF mid = "left_person"
                    THEN /\ right_door' = "open"
                         /\ right_door_opened_from' = "inside"
                         /\ last_door_opened_from' = "inside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << right_door, right_door_opened_from, 
                                         last_door_opened_from >>
              /\ UNCHANGED <<left_door, left_door_opened_from>>
        /\ pc' = "opened"
        /\ UNCHANGED << left, mid, right, unauthorized_mid_entry, 
                        unauthorized_mid_exit, stack >>

opened == /\ pc = "opened"
          /\ pc' = Head(stack).pc
          /\ stack' = Tail(stack)
          /\ UNCHANGED << left_door, right_door, left, mid, right, 
                          left_door_opened_from, right_door_opened_from, 
                          last_door_opened_from, unauthorized_mid_entry, 
                          unauthorized_mid_exit >>

open_door == check_openable \/ open \/ opened

close == /\ pc = "close"
         /\ \/ /\ IF left_door = "open"
                     THEN /\ left_door' = "closed"
                     ELSE /\ TRUE
                          /\ UNCHANGED left_door
               /\ UNCHANGED right_door
            \/ /\ IF right_door = "open"
                     THEN /\ right_door' = "closed"
                     ELSE /\ TRUE
                          /\ UNCHANGED right_door
               /\ UNCHANGED left_door
         /\ pc' = "closed"
         /\ UNCHANGED << left, mid, right, left_door_opened_from, 
                         right_door_opened_from, last_door_opened_from, 
                         unauthorized_mid_entry, unauthorized_mid_exit, stack >>

closed == /\ pc = "closed"
          /\ pc' = Head(stack).pc
          /\ stack' = Tail(stack)
          /\ UNCHANGED << left_door, right_door, left, mid, right, 
                          left_door_opened_from, right_door_opened_from, 
                          last_door_opened_from, unauthorized_mid_entry, 
                          unauthorized_mid_exit >>

close_door == close \/ closed

who_walks == /\ pc = "who_walks"
             /\ IF mid = "left_person" /\ right_door = "open"
                   THEN /\ right' = mid
                        /\ mid' = "empty"
                        /\ IF right_door_opened_from = "outside"
                              THEN /\ unauthorized_mid_exit' = TRUE
                              ELSE /\ unauthorized_mid_exit' = FALSE
                        /\ UNCHANGED << left, unauthorized_mid_entry >>
                   ELSE /\ IF mid = "right_person" /\ left_door = "open"
                              THEN /\ left' = mid
                                   /\ mid' = "empty"
                                   /\ IF left_door_opened_from = "outside"
                                         THEN /\ unauthorized_mid_exit' = TRUE
                                         ELSE /\ unauthorized_mid_exit' = FALSE
                                   /\ UNCHANGED << right, 
                                                   unauthorized_mid_entry >>
                              ELSE /\ IF mid = "empty"
                                         THEN /\ IF left = "left_person" /\ left_door = "open"
                                                    THEN /\ mid' = left
                                                         /\ left' = "empty"
                                                         /\ IF left_door_opened_from = "inside"
                                                               THEN /\ unauthorized_mid_entry' = TRUE
                                                               ELSE /\ unauthorized_mid_entry' = FALSE
                                                         /\ right' = right
                                                    ELSE /\ IF right = "right_person" /\ right_door = "open"
                                                               THEN /\ mid' = right
                                                                    /\ right' = "empty"
                                                                    /\ IF right_door_opened_from = "inside"
                                                                          THEN /\ unauthorized_mid_entry' = TRUE
                                                                          ELSE /\ unauthorized_mid_entry' = FALSE
                                                               ELSE /\ TRUE
                                                                    /\ UNCHANGED << mid, 
                                                                                    right, 
                                                                                    unauthorized_mid_entry >>
                                                         /\ left' = left
                                         ELSE /\ TRUE
                                              /\ UNCHANGED << left, mid, right, 
                                                              unauthorized_mid_entry >>
                                   /\ UNCHANGED unauthorized_mid_exit
             /\ pc' = Head(stack).pc
             /\ stack' = Tail(stack)
             /\ UNCHANGED << left_door, right_door, left_door_opened_from, 
                             right_door_opened_from, last_door_opened_from >>

walk == who_walks

main == /\ pc = "main"
        /\ \/ /\ stack' = << [ procedure |->  "new_arrival",
                               pc        |->  "main" ] >>
                           \o stack
              /\ pc' = "arrive"
           \/ /\ stack' = << [ procedure |->  "person_leaves",
                               pc        |->  "main" ] >>
                           \o stack
              /\ pc' = "depart"
           \/ /\ stack' = << [ procedure |->  "open_door",
                               pc        |->  "main" ] >>
                           \o stack
              /\ pc' = "check_openable"
           \/ /\ stack' = << [ procedure |->  "close_door",
                               pc        |->  "main" ] >>
                           \o stack
              /\ pc' = "close"
           \/ /\ stack' = << [ procedure |->  "walk",
                               pc        |->  "main" ] >>
                           \o stack
              /\ pc' = "who_walks"
        /\ UNCHANGED << left_door, right_door, left, mid, right, 
                        left_door_opened_from, right_door_opened_from, 
                        last_door_opened_from, unauthorized_mid_entry, 
                        unauthorized_mid_exit >>

Next == new_arrival \/ person_leaves \/ open_door \/ close_door \/ walk
           \/ main

Spec == Init /\ [][Next]_vars

\* END TRANSLATION 


\* Write down useful invariants here:
\* Both doors cannot be open at the same time.
inv0 == ~(left_door = "open" /\ right_door = "open")

\* Check if someone sneaked into the middle, and check if someone from the middle left if the door was opened from the outside.
inv1 == ~(unauthorized_mid_entry = TRUE /\ unauthorized_mid_exit = TRUE)


(*
According to the test the hypothesis from FormalSecure was correct.
It appears an unauthorized person can enter the middle once an authorized person leaves the middle and the door stays open.
Once in the middle they can exit if a person from the other side opens the other door.

The propossed solution of FormalSecure works in keeping unauthorized people out.
But it still allows someone from the outside to open the door for someone that is in the middle.
Only if that person in the middle entered the middle by opening a outside door.
So an unauthorized person can still enter the middle once someone opens the door from the outside, by either cutting in front and wait untill someone from the other side opens the other door.

Besides this, the person in the middle is stuck if they sneak in after someone who left the door open and closes the door behind them when they are in the middle.
This is because they cannot open the doors themself and the person from the outside cannot open the door because the last door opened was from the inside.
So the middle person is stuck and the door does not work anymore.
*)

============================================================================
