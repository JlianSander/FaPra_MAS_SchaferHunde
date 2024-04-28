!start.

+!start : true
    <- place_sheep;
        getOwnID(MyID);
       +myID(MyID);
       getOwnLocation(X,Y);
       -+pos(X,Y);
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

+agentMoved(ID, X, Y) 
    :  myID(MyID) &
    MyID = ID 
    <- 
    -+pos(X,Y).

+pos(X,Y) <- 
    ?myID(ID);
    .print(ID," moves to: (", X," , ", Y, ")").

+!moveRightContinuously
    <- dstar;
       .wait(100);
       !moveRightContinuously.

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }