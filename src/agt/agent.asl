!start.

+!start : .my_name(Me)
    <- initAgent(Me, AgX, AgY);
        +pos(AgX,AgY);
        .print(Me, " initialized at (", AgX, " , ", AgY, ")");
        !init.

+pos(X,Y) <- 
    !broadCastPos(X,Y).

// ----- broadcastPos ----- //

+!broadCastPos(X,Y) <- 
    .my_name(ID);
    .broadcast(achieve, trackMove(X,Y));
    .print(ID," moves to: (", X," , ", Y, ")").