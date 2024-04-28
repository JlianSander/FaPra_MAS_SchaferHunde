!start.

+!start : true
    <- place_sheep;
        getOwnID(MyID);
       +myID(MyID);
       getOwnLocation(X,Y);
       -+pos(X,Y);
       .wait(500);
       .my_name(Me);
       .broadcast(tell, sheep(Me));
       .print("Sheep initialized. ID: " , MyID);
    //    !moveRightContinuously.
        !flock;
        .

+!flock : true
    <- 
    .print("hi");
    jia.flocking_pos(TargetX, TargetY);
    .print("hi2");
    .print("Target: (", TargetX, " , ", TargetY, ")");
    moveStep(TargetX, TargetY);
    .


+!moveStep(X, Y) : true
    <- 
    +formerPos(MyX, MyY);
    moveTo(X,Y);
    -+pos(X, Y);
    .wait(100);
    !flock.

+pos(X,Y) <- 
    !broadCastPos(X,Y).

+!broadCastPos(X,Y) <- 
    ?myID(ID);
    .broadcast(achieve, trackMove(X,Y));
    .print(ID," moves to: (", X," , ", Y, ")").

+!moveRightContinuously : jammed_counter(J) & J < 30
     <- 
    .print("start moveRightContinuously jammed: ", J);
    dstar(NewX, NewY);
    -+pos(NewX, NewY);
    .wait(300);
    !moveRightContinuously.

-!moveRightContinuously <- !retryMoving.

+!retryMoving : jammed_counter(J) & J < 30
    <-
    ?jammed_counter(J);
    .print("start retryMoving jammed: ", J);
    -+jammed_counter(J + 1);
    .wait(300);
    !moveRightContinuously.

+!retryMoving <- true.

+!trackMove(X, Y)
    <- true.

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }