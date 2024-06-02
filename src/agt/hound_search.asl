+!search_sheep <- .print("Benni Searching for sheep flock.");
                  !random_walk. //Später get_nearest_sheep_in_range

//------------------------------------------------------- random_walk -------------------------------------------------------

+!random_walk <- .print("Benni Randomly walking to search for sheep.");
                  ?pos(AgX, AgY);
                  !get_random_position(NewX, NewY); //AgX wird zu NewX, AgY => NewY
                  .print("Benni Walking to random position: (", NewX, ",", NewY, ")");
                  !reachRandomDestination(NewX, NewY).

//------------------------------------------------------- reachRandomDestination -------------------------------------------------------

+!reachRandomDestination(X, Y) : not pos(X, Y) // not yet reached target coordinates
    <- .print("Benni walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachRandomDestination(X, Y).

//searching flock > 3 sheeps
+!reachRandomDestination(X, Y) : pos(X, Y) 
    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");
       ?pos(AgX, AgY);
       !get_nearest_sheep_in_range(AgX, AgY, 0, 0, 7, NearestX, NearestY); 
       ( (NearestX \== -1) & (NearestY \== -1) )
       .print("Benni Found sheep flock at: (", NearestX, ",", NearestY, ")");
       !move_to_sheep(NearestX, NearestY);
       !random_walk.

+!move_to_sheep(X, Y) <- .print("Benni moving to sheep at: (", X, ",", Y, ")");
                         !reachDestination(X, Y).

+!reachDestination(X, Y) : not pos(X, Y)
    <- .print("walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachDestination(X, Y).

       +!reachDestination(X, Y) : pos(X, Y)
    <- .print("Reached destination: (", X, ",", Y, "), starting search for new flock.");
       !random_walk.

//end searching flock >3 sheeps

//+!reachRandomDestination(X, Y) : pos(X, Y) // reached target coordinates
//    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");
//       !random_walk.

//////////////////////////////////////////////////////////////////////////////////////////////////// Helper Actions ////////////////////////////////////////////////////////////////////////////////////////////////////

+!makeStepTowards(X, Y) <- nextStep(X, Y, NewX, NewY); !updatePos(NewX, NewY). //nextStep zum Updaten neuer Position

+!updatePos(X, Y) : not last_step_not_OK <- -+pos(X, Y).  //Abfangbedingung

+!updatePos(X, Y) : last_step_not_OK <- -last_step_not_OK; .print("Benni last step not ok"). //Abfangbedingung

+!get_random_position(X, Y) <- jia.random_position(X, Y). // Versuch java random_position
