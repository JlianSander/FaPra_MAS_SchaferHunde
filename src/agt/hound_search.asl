+!startSearch
    <- .print("Search started");
    !!setMove;
    .

+!setMove
    <-
    !setTarget(10, 10);
    !setMove;
    .

+!setTarget(TargetX, TargetY)
    <-
    +destination(TargetX, TargetY);
    !reachDestination(TargetX, TargetY);
    .
