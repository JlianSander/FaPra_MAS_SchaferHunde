+!init : true
    <- .my_name(Me);
       .broadcast(tell, sheep(Me));
    //    !flock
    !move;
       .

+!flock : pos(AgX, AgY)
    <- 
    jia.flocking_pos(AgX, AgY, TargetX, TargetY);
    .print("Target: (", TargetX, " , ", TargetY, ")");
    +destination(TargetX, TargetY);
    !move;
    // !flock;
    .

+pos(X,Y) : formerPos(FX, FY) &
            ( X = FX  &  Y = FY)       //counter is not set
        <-
        -formerPos(FX,FY).

+pos(X,Y) : formerPos(FX, FY) &
            ( X = FX  &  Y = FY)
        <-
        -formerPos(FX,FY).

+!move : destination(X,Y) & 
    pos(MyX,MyY) &
    not ( MyX = X  &  MyY = Y)       //only take this plan if you haven't reached destination
    <- 
    // .print("start move 2 to (", X, ",", Y,")");
    -+formerPos(MyX, MyY);
    nextStep(X,Y, NewX, NewY);
    // .print("set new pos (",NewX,",",NewY,")");
    -+pos(NewX, NewY);
    .wait(100);
    !move.

+!move : destination(X,Y) & 
    pos(MyX,MyY) &
    ( MyX = X  &  MyY = Y)       //only take this plan if you have reached destination
    <- 
    .print("reached destination");
    -destination(X,Y);
    // !flock.
    .

// +!moveStep(X, Y) : pos(AgX, AgY)
//     <- 
//     // +formerPos(AgX, AgY);
//     nextStep(X, Y, NewX, NewY);
//     -+pos(NewX, NewY);
//     .wait(100);
//     .

+!trackMove(X, Y)
    <- true.

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }
{ include("agent.asl") }