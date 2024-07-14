+!init
    <- .my_name(Me);
       .broadcast(tell, sheep(Me));
       +formerPos(-1, -1);
       !!flock;
       .

failed(0).
       
is_in_corral :- pos(AgX, AgY) & jia.common.is_in_corral(AgX, AgY).

target_not_reached(AgX, AgY, TargetX, TargetY) :- pos(AgX, AgY) &
            destination(TargetX, TargetY) &
            formerPos(FormerX, FormerY) &
            not (AgX = TargetX  &  AgY = TargetY).

is_done :- pos(AgX, AgY) &
            formerPos(FormerAgX, FormerAgY) &
            destination(TargetX, TargetY) &
            ( AgX = TargetX  &  AgY = TargetY) &
            not (AgX = FormerAgX  &  AgY = FormerAgY).

do_flock(AgX, AgY) :- doflock & pos(AgX, AgY).

+!flock : do_flock(AgX, AgY)
    <- 
    if(jia.sheep.flocking_pos(TargetX, TargetY)) {
        .print("Calculated new flocking pos. Start: ", AgX, " , ", AgY, " - Target: (", TargetX, " , ", TargetY, ")");
        !!setTarget(TargetX, TargetY);
    } else {
        !stuckSleep;
        !!flock;
    }
    .

+!setTarget(TargetX, TargetY)
    <-
    +destination(TargetX, TargetY);
    !takeStep;
    .

+!takeStep : is_in_corral
    <-
    sheepCaptured;
    jia.util.common.kill_and_decommission_agent;
    .

+!takeStep : target_not_reached(AgX, AgY, TargetX, TargetY)
    <-
    // .print("Destination not reached yet");
    -+formerPos(AgX, AgY);
    nextStep(TargetX, TargetY, NewX, NewY);
    // .print("Old pos: (", AgX, " , ", AgY, ") - New pos: (", NewX, " , ", NewY, ")");
    -+pos(NewX, NewY);
    -+failed(0);
    !waitToMove;
    !takeStep;
    .

+!takeStep : is_done
    <-
    // .print("Im Done!");
    -destination(X,Y);
    -+failed(0);
    !!flock.

-!takeStep : failed(F)
    <-
    .print("ABORT :(");
    -destination(X,Y);
    -formerPos(X,Y);
    if(F < 2) {
        -+failed(F + 1);
        .print("I failed ", F + 1, " times in a row. Trying again");
    } else {
        !stuckSleep;
    }
    !!flock.

+!stuckSleep : waitTime(Wait)
    <-
    .print("I'm stuck! I'll sleep for a while");
    .wait(Wait * 10);
    .

{ include("agent.asl") }