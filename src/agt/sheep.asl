+!init
    <- .my_name(Me);
       .broadcast(tell, sheep(Me));
       +formerPos(-1, -1);
       !!flock;
       .
       
is_in_corral :- pos(AgX, AgY) & jia.is_in_corral(AgX, AgY).

target_not_reached(AgX, AgY, TargetX, TargetY) :- pos(AgX, AgY) &
            destination(TargetX, TargetY) &
            formerPos(FormerX, FormerY) &
            not (AgX = TargetX  &  AgY = TargetY).

// is_stuck :- pos(AgX, AgY) &
//             formerPos(FormerAgX, FormerAgY) &
//             destination(TargetX, TargetY) &
//             (AgX = FormerAgX  &  AgY = FormerAgY) &
//             not ( AgX = TargetX  &  AgY = TargetY).

is_done :- pos(AgX, AgY) &
            formerPos(FormerAgX, FormerAgY) &
            destination(TargetX, TargetY) &
            ( AgX = TargetX  &  AgY = TargetY) &
            not (AgX = FormerAgX  &  AgY = FormerAgY).

do_flock(AgX, AgY) :- doflock & pos(AgX, AgY).

+!flock : do_flock(AgX, AgY)
    <- 
    jia.flocking_pos(AgX, AgY, TargetX, TargetY);
    .my_name(Me);
    .print("Calculated new flocking pos. Start: ", AgX, " , ", AgY, " - Target: (", TargetX, " , ", TargetY, ")");
    !!setTarget(TargetX, TargetY);
    .

+!setTarget(TargetX, TargetY)
    <-
    +destination(TargetX, TargetY);
    !takeStep;
    .


+!takeStep : is_in_corral
    <-
    .print("I'm in the corral");
    sheepCaptured;
    jia.util.kill_and_decommission_agent;
    .

+!takeStep : target_not_reached(AgX, AgY, TargetX, TargetY)
    <-
    .print("Destination not reached yet");
    -+formerPos(AgX, AgY);
    nextStep(TargetX, TargetY, NewX, NewY);
    .print("Old pos: (", AgX, " , ", AgY, ") - New pos: (", NewX, " , ", NewY, ")");
    -+pos(NewX, NewY);
    !waitToMove;
    !takeStep;
    .

// +!takeStep : is_stuck 
//     <-
//     .print("IM STUCK!");
//     -destination(X,Y);
//     !!flock.

+!takeStep : is_done
    <-
    .print("Im Done!");
    -destination(X,Y);
    !!flock.

-!takeStep
    <-
    .print("ABORT :(");
    -destination(X,Y);
    -formerPos(X,Y);
    !!flock.

{ include("agent.asl") }