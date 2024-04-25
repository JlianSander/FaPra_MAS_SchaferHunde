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
    <- !observe(ID, X, Y).

+agentMoved(ID, X, Y) 
    :  myID(MyID) &
    MyID = ID 
    <- 
    -+pos(X,Y).



+!observe(ID, X, Y) <- 
    -+pos_agent(X ,Y)[agent_ID(ID)];
    .print("observing ", ID).


+!explore <- 
    .wait(400);
    ?destination(X,Y);
    !move(X,Y).
    
+!move(X, Y) 
    : not formerPos(FX, FY) &       //only take this plan if you haven't moved yet
      pos(MyX,MyY) &
      not ( MyX = X  &  MyY = Y)       //only take this plan if you haven't reached destination
    <- 
    +formerPos(MyX, MyY);
    nextStep(X,Y, NewX, NewY);
    -+pos(NewX, NewY);
    .wait(100);
    !move(X,Y).

+!move(X, Y) 
    : formerPos(FX, FY) &
      pos(MyX,MyY) &
      not ( MyX = X  &  MyY = Y) &       //only take this plan if you haven't reached destination
      not ( MyX = FX  &  MyY = FY)       //only take this plan if you're not stepping on same spot
      <- 
    -+formerPos(MyX, MyY);
    nextStep(X,Y, NewX, NewY);
    -+pos(NewX, NewY);
    .wait(100);
    !move(X,Y).

+!move(X, Y)<-
    -formerPos(FX, FY);                 //reset belief about former position
    .print("stop moving").

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }