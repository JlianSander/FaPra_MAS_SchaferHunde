+!startSearch
    <- .print("Search started");
    !!setMove;
    .

+!setMove : not (.desire(reachDestination(L,M)) & (L \== X | M \== Y))
    <-
    jia.get_random_position(TargetX, TargetY);
    !setTarget(TargetX, TargetY);
    !setMove;
    .

+!setMove : .desire(reachDestination(L,M)) & (L \== X | M \== Y) 
    <- 
    jia.check_nearby_sheep(SheepX, SheepY);
    if(SheepX \== -1){
        .print("Sheep found at ", SheepX, ",", SheepY, " - passing to drive");
        .drop_desire(walkTowards(L,M));
        .drop_desire(reachDestination(L,M));
        !!startDrive;
    } else {
        !waitToMove;
        !setMove;
    }
    .

+!setTarget(TargetX, TargetY)
    <-
    !!reachDestination(TargetX, TargetY);
    .
