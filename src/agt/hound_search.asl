// Hauptplan zur Suche nach Schafen
+!search_sheep <- .print("Benni Searching for sheep flock.");   // Goal "search_sheep"
                  !random_walk. //Später get_nearest_sheep_in_range   //aufruf "random_walk"

//------------------------------------------------------- random_walk -------------------------------------------------------

// Plan für zufälliges Herumlaufen
+!random_walk <- .print("Benni Randomly walking to search for sheep.");
                  ?pos(AgX, AgY);
                  !get_random_position(NewX, NewY); //AgX wird zu NewX, AgY => NewY
                  .print("Benni Walking to random position: (", NewX, ",", NewY, ")");
                  !reachRandomDestination(NewX, NewY).

//------------------------------------------------------- reachRandomDestination -------------------------------------------------------

// Plan für das Erreichen eines zufälligen Ziels
+!reachRandomDestination(X, Y) : not pos(X, Y) // not yet reached target coordinates
    <- .print("Benni walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachRandomDestination(X, Y).

/*
//searching flock => get_nearest_sheep_in_range
+!reachRandomDestination(X, Y) : pos(X, Y) 
    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");
       ?pos(AgX, AgY);
       !jia.get_nearest_sheep_in_range(AgX, AgY, 0, 0, 7, NearestX, NearestY);
       if ( (NearestX \== -1) & (NearestY \== -1) ) {
           .print("Benni Found sheep flock at: (", NearestX, ",", NearestY, ")");
           !start_drive;
       } else {
           !random_walk
       }.

//searching sheep => get_nearest_sheep_in_range_2

// Plan für das Finden der Herde und das Starten von hound_drive
+!reachRandomDestination(X, Y) : pos(X, Y)   //Abruf Position des Hundes 
    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");  
       ?pos(AgX, AgY);
       jia.get_nearest_sheep_in_range(AgX, AgY, 0, 0, 7, NearestX, NearestY);   //Abfrage ob Schwarm im Umkreis von 7 Felder ist.
       if ( (NearestX \== -1) & (NearestY \== -1) ) {   // -1 kein Schwarm gefunden, nicht -1 Schwarm gefunden
           !random_walk //Kein Schwarm gefunden und hound_drive wird 
       } else {
            .print("Benni Found sheep flock at: (", NearestX, ",", NearestY, ")");
            !start_drive; //Schwarm gefunden und hound_drive wird 
       }.



// Aufrufen get_nearest_sheep_in_range => searching sheep flock
+!reachRandomDestination(X, Y) : pos(X, Y)   //Abruf Position des Hundes 
    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");  
       ?pos(AgX, AgY);
       jia.get_nearest_sheep_in_range(AgX, AgY, 0, 0, 10, NearestX, NearestY);   //Abfrage ob Schwarm im Umkreis von 7 Felder ist.
       .print("Benni Nearest sheep flock coordinates: (", NearestX, ", ", NearestY, ") // Result from jia.get_nearest_sheep_in_range: NearestX=", NearestX, ", NearestY=", NearestY);
       if ( (NearestX == -1) & (NearestY == -1) ) {   // -1 kein Schwarm gefunden, nicht -1 Schwarm gefunden
           !random_walk; //Kein Schwarm gefunden und hound_drive wird 
       } else {
            //.print("Benni Found sheep flock at: (", NearestX, ",", NearestY, ")");
            !start_drive; //Schwarm gefunden und hound_drive wird 
       }.

*/


+!reachRandomDestination(X, Y) : pos(X, Y) 
    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");
       ?pos(AgX, AgY);  // Aktuelle Position des Agenten abfragen
       jia.get_nearest_sheep_in_range_2(AgX, AgY, 0, 0, 10, NearestX, NearestY);  // Aufruf der Internal Action zur Suche nach Schafen
       .print("Benni Nearest sheep coordinates received: (", NearestX, ",", NearestY, ")");
       if ((NearestX == -1) & (NearestY == -1)) {  // Keine Schafe gefunden
           .print("Benni No sheep in range, continuing to random walk.");
           !random_walk;  // Fortsetzung random_walk
       } else {  // Schafe gefunden
           .print("Benni Found sheep at: (", NearestX, ",", NearestY, "), starting drive.");
           !start_drive;  // Start zum Treiben der Schafe
       }.



// Plan zum Erreichen des Ziels
+!reachDestination(X, Y) : not pos(X, Y)
    <- .print("walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachDestination(X, Y).

       +!reachDestination(X, Y) : pos(X, Y)
    <- .print("Reached destination: (", X, ",", Y, "), starting search for new flock.");
       !random_walk.


+!reachRandomDestination(X, Y) : pos(X, Y) // reached target coordinates
    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");
       !random_walk.

//////////////////////////////////////////////////////////////////////////////////////////////////// Helper Actions ////////////////////////////////////////////////////////////////////////////////////////////////////

+!makeStepTowards(X, Y) <- nextStep(X, Y, NewX, NewY); !updatePos(NewX, NewY). //nextStep zum Updaten neuer Position

+!updatePos(X, Y) : not last_step_not_OK <- -+pos(X, Y).  //Abfangbedingung

+!updatePos(X, Y) : last_step_not_OK <- -last_step_not_OK; .print("Benni last step not ok"). //Abfangbedingung

+!get_random_position(X, Y) <- jia.random_position(X, Y). // Versuch java random_position

//+!get_nearest_sheep_in_range(AgX, AgY, Arg3, Arg4, Range, NearestX, NearestY) 
//    <- .print("Benni Attempting to find nearest sheep in range.");
//       jia.get_nearest_sheep_in_range(AgX, AgY, Arg3, Arg4, Range, NearestX, NearestY);
//       .print("Benni Nearest sheep coordinates: ", NearestX, ", ", NearestY).
