//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////

+!init : true
    <- .my_name(Me);
       .broadcast(tell, hound(Me)).

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

//------------------------------------------------------- pos -------------------------------------------------------
+pos(X,Y) : formerPos(FX, FY) &
            ( X = FX  &  Y = FY) &      //new position is same than old one = is jammed
            not jammed_counter(J)       //counter is not set
        <-
        -formerPos(FX,FY);
        +jammed_counter(1);
        .print("jammed counter: " , 1).

+pos(X,Y) : formerPos(FX, FY) &
            ( X = FX  &  Y = FY) &      //new position is same than old one = is jammed
            jammed_counter(J)           //counter is set
        <-
        -formerPos(FX,FY);
        -+jammed_counter(J + 1);
        .print("jammed counter: " , J + 1).


//------------------------------------------------------- move -------------------------------------------------------
+!move : jammed_counter(J) &
    J > 10                          //agent is jammed for too long, abort moving
    <-
    .print("start move 1");
    -jammed_counter(J);
    .print("jammed").

+!move : destination(X,Y) & 
    pos(MyX,MyY) &
    not ( MyX = X  &  MyY = Y)       //only take this plan if you haven't reached destination
    <- 
    .print("start move 2 to (", X, ",", Y,")");
    -+formerPos(MyX, MyY);
    nextStep(X,Y, NewX, NewY);
    .print("set new pos (",NewX,",",NewY,")");
    -+pos(NewX, NewY);
    .wait(100);
    !move.

+!move : destination(X,Y) & 
    pos(MyX,MyY) &
    ( MyX = X  &  MyY = Y)       //only take this plan if you have reached destination
    <- 
    .print("start move 3");
    -jammed_counter(J);
    .print("reached destination").

-!move <-                       //exception plan, if movement fails or gets aborted
    -jammed_counter(J).                 

//------------------------------------------------------- observe -------------------------------------------------------
+!observe(A, X, Y) <- 
    -+pos_agent(X ,Y)[source(A)];
    -+destination(X,Y);
    .print("observing ", A);
    .drop_intention(move);
    !move.

//------------------------------------------------------- trackMove -------------------------------------------------------
+!trackMove(X, Y)[source(A)]
    : A \== self &
    pos(MyX,MyY) &
    X <= MyX + 5 &
    X >= MyX - 5 &
    Y <= MyY + 5 &
    Y >= MyY - 5
    <- !observe(A, X, Y).

+!trackMove(X,Y) <- true.
//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }
{ include("agent.asl") }