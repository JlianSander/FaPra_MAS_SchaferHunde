+modelChanged(X) <- //.print("world changed").
    !observe.

+!observe <- .print("observing");
    //get current position
    //.getVisibleAgts(LOC, 2); //get visible agents of current position
    //add beliefs for every agent and his current position
    .


/*+!explore(DIR) <- ;
    //move along the direction
    .
*/
{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }