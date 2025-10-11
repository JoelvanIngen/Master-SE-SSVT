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
             person_who_moved = "none";
             door_opened_from = "none";
    
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
            person_who_moved := "none";
            open: 
            either
                if (left = "left_person" /\ door_opened_from # "inside" /\ mid = "empty") {
                    left_door := "open";
                    door_opened_from := "outside";
                };
            or
                if (mid = "right_person") {
                    left_door := "open";
                    door_opened_from := "inside";
                };
            or
                if (right = "right_person" /\ door_opened_from # "inside" /\ mid = "empty") {
                    right_door := "open";
                    door_opened_from := "outside";
                };
            or 
                if (mid = "left_person") {
                    right_door := "open";
                    door_opened_from := "inside";
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
                    person_who_moved := "mid";
                    };
        else if (mid = "right_person" /\ left_door = "open") {
                    left := mid;
                    mid := "empty";
                    person_who_moved := "mid";
                    };
        else if (mid = "empty") {
            if (left = "left_person" /\ left_door = "open") {
                    mid := left;
                    left := "empty";
                    person_who_moved := "left";
                    };
            else if (right = "right_person" /\ right_door = "open") {
                    mid := right;
                    right := "empty";
                    person_who_moved := "right";
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
\* BEGIN TRANSLATION (chksum(pcal) = "3b11cb1c" /\ chksum(tla) = "d62f013c")
VARIABLES pc, left_door, right_door, left, mid, right, person_who_moved, 
          door_opened_from, stack

vars == << pc, left_door, right_door, left, mid, right, person_who_moved, 
           door_opened_from, stack >>

Init == (* Global variables *)
        /\ left_door = "closed"
        /\ right_door = "closed"
        /\ left = "empty"
        /\ mid = "empty"
        /\ right = "empty"
        /\ person_who_moved = "none"
        /\ door_opened_from = "none"
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
          /\ UNCHANGED << left_door, right_door, mid, person_who_moved, 
                          door_opened_from >>

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
          /\ UNCHANGED << left_door, right_door, mid, person_who_moved, 
                          door_opened_from, stack >>

departed == /\ pc = "departed"
            /\ pc' = Head(stack).pc
            /\ stack' = Tail(stack)
            /\ UNCHANGED << left_door, right_door, left, mid, right, 
                            person_who_moved, door_opened_from >>

person_leaves == depart \/ departed

check_openable == /\ pc = "check_openable"
                  /\ IF left_door = "closed" /\ right_door = "closed"
                        THEN /\ person_who_moved' = "none"
                             /\ pc' = "open"
                        ELSE /\ pc' = "opened"
                             /\ UNCHANGED person_who_moved
                  /\ UNCHANGED << left_door, right_door, left, mid, right, 
                                  door_opened_from, stack >>

open == /\ pc = "open"
        /\ \/ /\ IF left = "left_person" /\ door_opened_from # "inside" /\ mid = "empty"
                    THEN /\ left_door' = "open"
                         /\ door_opened_from' = "outside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << left_door, door_opened_from >>
              /\ UNCHANGED right_door
           \/ /\ IF mid = "right_person"
                    THEN /\ left_door' = "open"
                         /\ door_opened_from' = "inside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << left_door, door_opened_from >>
              /\ UNCHANGED right_door
           \/ /\ IF right = "right_person" /\ door_opened_from # "inside" /\ mid = "empty"
                    THEN /\ right_door' = "open"
                         /\ door_opened_from' = "outside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << right_door, door_opened_from >>
              /\ UNCHANGED left_door
           \/ /\ IF mid = "left_person"
                    THEN /\ right_door' = "open"
                         /\ door_opened_from' = "inside"
                    ELSE /\ TRUE
                         /\ UNCHANGED << right_door, door_opened_from >>
              /\ UNCHANGED left_door
        /\ pc' = "opened"
        /\ UNCHANGED << left, mid, right, person_who_moved, stack >>

opened == /\ pc = "opened"
          /\ pc' = Head(stack).pc
          /\ stack' = Tail(stack)
          /\ UNCHANGED << left_door, right_door, left, mid, right, 
                          person_who_moved, door_opened_from >>

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
         /\ UNCHANGED << left, mid, right, person_who_moved, door_opened_from, 
                         stack >>

closed == /\ pc = "closed"
          /\ pc' = Head(stack).pc
          /\ stack' = Tail(stack)
          /\ UNCHANGED << left_door, right_door, left, mid, right, 
                          person_who_moved, door_opened_from >>

close_door == close \/ closed

who_walks == /\ pc = "who_walks"
             /\ IF mid = "left_person" /\ right_door = "open"
                   THEN /\ right' = mid
                        /\ mid' = "empty"
                        /\ person_who_moved' = "mid"
                        /\ left' = left
                   ELSE /\ IF mid = "right_person" /\ left_door = "open"
                              THEN /\ left' = mid
                                   /\ mid' = "empty"
                                   /\ person_who_moved' = "mid"
                                   /\ right' = right
                              ELSE /\ IF mid = "empty"
                                         THEN /\ IF left = "left_person" /\ left_door = "open"
                                                    THEN /\ mid' = left
                                                         /\ left' = "empty"
                                                         /\ person_who_moved' = "left"
                                                         /\ right' = right
                                                    ELSE /\ IF right = "right_person" /\ right_door = "open"
                                                               THEN /\ mid' = right
                                                                    /\ right' = "empty"
                                                                    /\ person_who_moved' = "right"
                                                               ELSE /\ TRUE
                                                                    /\ UNCHANGED << mid, 
                                                                                    right, 
                                                                                    person_who_moved >>
                                                         /\ left' = left
                                         ELSE /\ TRUE
                                              /\ UNCHANGED << left, mid, right, 
                                                              person_who_moved >>
             /\ pc' = Head(stack).pc
             /\ stack' = Tail(stack)
             /\ UNCHANGED << left_door, right_door, door_opened_from >>

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
                        person_who_moved, door_opened_from >>

Next == new_arrival \/ person_leaves \/ open_door \/ close_door \/ walk
           \/ main

Spec == Init /\ [][Next]_vars

\* END TRANSLATION 


\* Write down useful invariants here:
\* Both doors cannot be open at the same time.
inv0 == ~(left_door = "open" /\ right_door = "open")
\* Check if the middle person doesn't move if someone from the outside opened to door.
inv1 == ~(person_who_moved = "mid" /\ door_opened_from = "outside")

(*

According to the test the hypothesis from FormalSecure was correct.
It appears an unauthorized person can enter the middle once an authorized person leaves the middle and the door stays open.
Once in the middle they can exit if a person from the other side opens the other door.

The propossed solution of FormalSecure works in keeping unauthorized people out.
It does however cause a new issue which is that the door is now forever stuck when someone tries to sneak in.
This is because the door cannot be opened from the outside and the person in the middle has no authorization to open the door from the inside.

A cheaper solution that does not require a sensor would be:

*)

============================================================================
