!start.

+!start : true
    <- place_sheep;
        getOwnID(MyID);
       +myID(MyID);
       getOwnLocation(X,Y);
       -+pos(X,Y);
       .print("Sheep initialized. ID: " , MyID);
       !moveRightContinuously.

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