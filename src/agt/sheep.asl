+!init : true
    <- .my_name(Me);
       .broadcast(tell, sheep(Me));
       !flock.

+!flock : pos(AgX, AgY)
    <- 
    jia.flocking_pos(AgX, AgY, TargetX, TargetY);
    .print("Target: (", TargetX, " , ", TargetY, ")");
    !moveStep(TargetX, TargetY);
    !flock.

+!moveStep(X, Y) : pos(AgX, AgY)
    <- 
    // +formerPos(AgX, AgY);
    nextStep(X, Y, NewX, NewY);
    -+pos(NewX, NewY);
    .wait(100);
    .

+!trackMove(X, Y)
    <- true.

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }
{ include("agent.asl") }