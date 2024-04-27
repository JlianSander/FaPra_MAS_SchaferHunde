//initial plans
!start.

+!start : true
    <- place_hound;
       getOwnID(MyID);
       +myID(MyID);
       getOwnLocation(X,Y);
       +pos(X,Y);
       .print("Hound initialized. ID: " , MyID).
       

+pos(X,Y) : formerPos(FX, FY) &
            ( X = FX  &  Y = FY) &      //new position is same than old one = is jammed
            not jammed_counter(J)       //counter is not set
        <-
        -formerPos(FX,FY);
        +jammed_counter(1);
        !debugPos.

+pos(X,Y) : formerPos(FX, FY) &
            ( X = FX  &  Y = FY) &      //new position is same than old one = is be jammed
            jammed_counter(J)           //counter is set
        <-
        -formerPos(FX,FY);
        -+jammed_counter(J + 1);
        !debugPos.

+pos(X,Y) <- 
    !debugPos.

+!debugPos
    <-
    ?myID(ID);
    ?pos(X,Y);
    .print(ID," moves to: (", X," , ", Y, ")").

+agentMoved(ID, X, Y) 
    :  myID(MyID) &
    not MyID = ID &
    pos(MyX,MyY) &
    X <= MyX + 2 &
    X >= MyX - 2 &
    Y <= MyY + 2 &
    Y >= MyY - 2
    <- !observe(ID, X, Y).

+agentMoved(ID, X, Y) 
    :  myID(MyID) &
    MyID = ID 
    <- 
    -+pos(X,Y).

+!observe(ID, X, Y) <- 
    -+pos_agent(X ,Y)[agent_ID(ID)];
    -+destination(X,Y);
    .print("observing ", ID);
    !move.

+!move : jammed_counter(J) &
    J > 10                          //agent is jammed for too long, abort moving
    <-
    -jammed_counter(J);
    .print("jammed").

+!move : destination(X,Y) & 
    pos(MyX,MyY) &
    not ( MyX = X  &  MyY = Y)       //only take this plan if you haven't reached destination
    <- 
    -+formerPos(MyX, MyY);
    nextStep(X,Y, NewX, NewY);
    -+pos(NewX, NewY);
    .wait(100);
    !move.

+!move : destination(X,Y) & 
    pos(MyX,MyY) &
    ( MyX = X  &  MyY = Y)       //only take this plan if you have reached destination
    <- .print("reached destination").

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }