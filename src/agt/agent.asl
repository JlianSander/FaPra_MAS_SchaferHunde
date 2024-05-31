//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////
!start.

repeated_pos(0).

+!start : .my_name(Me)
    <- initAgent(Me, AgX, AgY, Wait);
        +pos(AgX,AgY);
        +waitTime(Wait);
        .print(Me, " initialized at (", AgX, " , ", AgY, ") with wait time: ", Wait);
        !init.

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////

+pos(X,Y) <- 
    !broadCastPos(X,Y).

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- broadcastPos -------------------------------------------------------

+!broadCastPos(X,Y) <- 
    .my_name(ID);
    .broadcast(achieve, trackMove(X,Y));
    .print(ID," moves to: (", X," , ", Y, ")").

//------------------------------------------------------- updatePos -------------------------------------------------------   

+!updatePos(X,Y) : not last_step_not_OK <-  -+pos(X, Y).

+!updatePos(X,Y) : last_step_not_OK <- -last_step_not_OK; .print("last step not ok").


//------------------------------------------------------- waitToMove -------------------------------------------------------   

+!waitToMove : waitTime(Wait) <- .wait(Wait).

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }

//------------------------------------------------------- repeatPos -------------------------------------------------------  
//DEBUG: this plan should become obsolete, once the agents can actively perceive the environment they move to
// needed to tell hounds position of non-moving sheep
+!repeatPos : not pos(X,Y)
    <- .wait(1000);
    !!repeatPos.

+!repeatPos : pos(X,Y) & repeated_pos(I) & I < 1
    <- !broadCastPos(X,Y);
    -+repeated_pos(I + 1);
    .wait(1000);
    !!repeatPos.

+!repeatPos <- true.

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////