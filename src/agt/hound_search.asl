////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////

// No special initialization required

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

// No specific beliefs required

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- searchSheep -------------------------------------------------------

+!search_sheep <- .print("Searching for sheep flock.");
                  !random_walk.

//------------------------------------------------------- random_walk -------------------------------------------------------

+!random_walk <- .print("Randomly walking to search for sheep.");
                  ?pos(AgX, AgY);
                  !get_random_position(NewX, NewY);
                  .print("Walking to random position: (", NewX, ",", NewY, ")");
                  !reachDestination(NewX, NewY).

//------------------------------------------------------- reachDestination -------------------------------------------------------

+!reachDestination(X, Y) : not pos(X, Y) // not yet reached target coordinates
    <- .print("walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachDestination(X, Y).

+!reachDestination(X, Y) <- .print("reached destination, continuing search."); // reached target coordinates
                             !random_walk.

//////////////////////////////////////////////////////////////////////////////////////////////////// Helper Actions ////////////////////////////////////////////////////////////////////////////////////////////////////

+!makeStepTowards(X, Y) <- nextStep(X, Y, NewX, NewY); !updatePos(NewX, NewY).

+!updatePos(X, Y) : not last_step_not_OK <- -+pos(X, Y).

+!updatePos(X, Y) : last_step_not_OK <- -last_step_not_OK; .print("last step not ok").

+!get_random_position(X, Y) <- jia.random_position(X, Y). // Assuming you have a Java action to get a random position
