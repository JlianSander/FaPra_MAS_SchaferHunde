!start.

+!start : .my_name(Me)
    <- initAgent(Me, AgX, AgY, Wait);
        +pos(AgX,AgY);
        +waitTime(Wait);
        .print(Me, " initialized at (", AgX, " , ", AgY, ") with wait time: ", Wait);
        !init.

+pos(X,Y) <- 
    !broadCastPos(X,Y).

// ----- broadcastPos ----- //

+!broadCastPos(X,Y) <- 
    .my_name(ID);
    .broadcast(achieve, trackMove(X,Y));
    .print(ID," moves to: (", X," , ", Y, ")").

//------------------------------------------------------- updatePos -------------------------------------------------------   

+!updatePos(X,Y) : not last_step_not_OK <-  -+pos(X, Y).

+!updatePos(X,Y) : last_step_not_OK <- -last_step_not_OK; .print("last step not ok").

//------------------------------------------------------- wait -------------------------------------------------------   

+!waitToMove : waitTime(Wait) <- .wait(Wait).