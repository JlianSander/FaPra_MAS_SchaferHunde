!start.

+!start : true
    <- initAgent(AgX, AgY);
        +pos(AgX,AgY);
        .my_name(Me);
        .print(Me, " initialized at (", AgX, " , ", AgY, ")");
        !init.

+pos(X,Y) <- 
    !broadCastPos(X,Y).

// ----- broadcastPos ----- //

+!broadCastPos(X,Y) <- 
    .my_name(ID);
    .broadcast(achieve, trackMove(X,Y));
    .print(ID," moves to: (", X," , ", Y, ")").