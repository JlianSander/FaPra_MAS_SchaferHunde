+!init
    <- .my_name(Me);
       .broadcast(tell, sheep(Me));
       +formerPos(-1, -1);
       +doflock;
       !flock;
       .

+!flock : doflock & pos(AgX, AgY)
    <- 
    // pos(AgX, AgY);
    jia.flocking_pos(AgX, AgY, TargetX, TargetY);
    .my_name(Me);
    .print(Me, " pos: ", AgX, " , ", AgY, " - Target: (", TargetX, " , ", TargetY, ")");
    !setTarget(TargetX, TargetY);
    !flock;
    .

// +pos(X,Y) : formerPos(FX, FY) &
//             ( X = FX  &  Y = FY)       //counter is not set
//         <-
//         -formerPos(FX,FY).

// +pos(X,Y) : formerPos(FX, FY) &
//             ( X = FX  &  Y = FY)
//         <-
//         -formerPos(FX,FY).

+!setTarget(TargetX, TargetY) : true
    <-
    +destination(TargetX, TargetY);
    !takeStep;
    .

+!takeStep : pos(AgX, AgY) &
            destination(TargetX, TargetY) &
            formerPos(FormerX, FormerY) &
            not (AgX = TargetX  &  AgY = TargetY)
    <-
    // .print("Destination not reached yet");
    // .print("before step:");
    // .print("destination: (", TargetX, " , ", TargetY, ")");
    // .print("current pos: (", AgX, " , ", AgY, ")");
    // .print("former pos: (", FormerX, " , ", FormerY, ")");
    +formerPos(AgX, AgY);
    nextStep(TargetX, TargetY, NewX, NewY);
    -+pos(NewX, NewY);
    .wait(100);
    // .print("after step:");
    // .print("destination: (", TargetX, " , ", TargetY, ")");
    // .print("current pos: (", NewX, " , ", NewY, ")");
    // .print("former pos: (", AgX, " , ", AgY, ")");
    !takeStep;
    .

+!takeStep : pos(AgX, AgY) &
            formerPos(FormerAgX, FormerAgY) &
            (AgX = FormerAgX  &  AgY = FormerAgY)
    <-
    .print("IM STUCK!");
    -destination(X,Y);
    !flock.

+!takeStep : pos(AgX, AgY) &
            destination(TargetX, TargetY) &
            ( AgX = TargetX  &  AgY = TargetY)
    <-
    .print("Im Done!");
    -destination(X,Y);
    !flock.

-!takeStep
    <-
    .print("ABORT :(");
    -destination(X,Y);
    -formerPos(X,Y);
    !flock.

// +!move : destination(X,Y) & 
//     pos(AgX, AgY) &
//     not ( AgX = X  &  AgY = Y)       //only take this plan if you haven't reached destination
//     <- 
//     // .print("start move 2 to (", X, ",", Y,")");
//     -+formerPos(AgX, AgY);
//     nextStep(X,Y, NewX, NewY);
//     // .print("set new pos (",NewX,",",NewY,")");
//     -+pos(NewX, NewY);
//     .wait(100);
//     !move.

// +!move : destination(X,Y) & 
//     pos(AgX, AgY) &
//     ( AgX = X  &  AgY = Y)       //only take this plan if you have reached destination
//     <- 
//     .print("reached destination");
//     -destination(X,Y);
//     // !flock.
//     .

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