//initial plans
!start.

+!start : true
    <- place_hound;
       getOwnID(MyID);
       +myID(MyID);
       getOwnLocation(X,Y);
       +pos(X,Y);
       .print("Hound initialized. ID: " , MyID).
       


+pos(X,Y) <- 
    ?myID(ID);
    .print(ID," moves to: (", X," , ", Y, ")").


+agentMoved(ID, X, Y) 
    :  myID(MyID) &
    not MyID = ID &
    pos(MyX,MyY) &
    X <= MyX + 2 &
    X >= MyX - 2 &
    Y <= MyY + 2 &
    Y >= MyY - 2
    <- !observe(ID).

+agentMoved(ID, X, Y) 
    :  myID(MyID) &
    MyID = ID 
    <- 
    -+pos(X,Y).



+!observe(ID) <- .print("observing ", ID).


/*+!explore(DIR) <- ;
    //move along the direction
    .
*/
{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }