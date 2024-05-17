+!init
    <- .my_name(Me);
       .broadcast(tell, sheep(Me));
       +formerPos(-1, -1);
    //    +doflock;
       !!flock;
       .

+!flock : doflock & pos(AgX, AgY)
    <- 
    // pos(AgX, AgY);
    jia.flocking_pos(AgX, AgY, TargetX, TargetY);
    .my_name(Me);
    .print("Calculated new flocking pos. Start: ", AgX, " , ", AgY, " - Target: (", TargetX, " , ", TargetY, ")");
    !setTarget(TargetX, TargetY);
    // !flock;
    .

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
    .print("Destination not reached yet");
    // .print("before step:");
    // .print("destination: (", TargetX, " , ", TargetY, ")");
    // .print("current pos: (", AgX, " , ", AgY, ")");
    // .print("former pos: (", FormerX, " , ", FormerY, ")");
    -+formerPos(AgX, AgY);
    nextStep(TargetX, TargetY, NewX, NewY);
    .print("Old pos: (", AgX, " , ", AgY, ") - New pos: (", NewX, " , ", NewY, ")");
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
            destination(TargetX, TargetY) &
            (AgX = FormerAgX  &  AgY = FormerAgY) &
            not ( AgX = TargetX  &  AgY = TargetY)
    <-
    .print("IM STUCK!");
    -destination(X,Y);
    !!flock.

+!takeStep : pos(AgX, AgY) &
            formerPos(FormerAgX, FormerAgY) &
            destination(TargetX, TargetY) &
            ( AgX = TargetX  &  AgY = TargetY) &
            not (AgX = FormerAgX  &  AgY = FormerAgY)
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

+!trackMove(X, Y)
    <- true.

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }
{ include("agent.asl") }