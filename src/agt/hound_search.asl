// Hauptplan zur Suche nach Schafen
+!search_sheep <- .print("Benni Searching for sheep flock.");   // Goal "search_sheep"
                  !random_walk.  

//------------------------------------------------------- random_walk -------------------------------------------------------

// Plan für random rumlaufen
+!random_walk <- .print("Benni Randomly walking to search for sheep.");
                  ?pos(AgX, AgY);
                  !get_random_position(NewX, NewY); // AgX wird zu NewX, AgY => NewY
                  .print("Benni Walking to random position: (", NewX, ",", NewY, ")");
                  !reachRandomDestination(NewX, NewY).

//------------------------------------------------------- reachRandomDestination -------------------------------------------------------

// Plan für das Erreichen eines zufälligen Ziels
+!reachRandomDestination(X, Y) : not pos(X, Y) // Noch nicht Ziel erreicht
    <- .print("Benni walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachRandomDestination(X, Y).

// Plan für das Handling des "jammed"-Zustands, wenn das Bewegen fehlschlägt
-!makeStepTowards(X, Y) : is_jammed
    <- .print("Benni is jammed, starting sheep search again.");
       !search_sheep. // Starte die Suche nach Schafen erneut


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
           !!startDrive;  // Start zum Treiben der Schafe
       }.

//------------------------------------------------------- reachDestination -------------------------------------------------------

+!reachDestination(X, Y) : not pos(X, Y)
    <- .print("walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachDestination(X, Y).

+!reachDestination(X, Y) : pos(X, Y)
    <- .print("Reached destination: (", X, ",", Y, "), starting search for new flock.");
       !random_walk.

//------------------------------------------------------- New Handling for Jammed State -------------------------------------------------------

+!reachRandomDestination(X, Y) : pos(X, Y) // Zielkoordinaten erreicht
    <- .print("Benni Reached destination at: (", X, ",", Y, "), continuing search.");
       !random_walk.

//////////////////////////////////////////////////////////////////////////////////////////////////// Helper Actions ////////////////////////////////////////////////////////////////////////////////////////////////////

+!makeStepTowards(X, Y) <- nextStep(X, Y, NewX, NewY); !updatePos(NewX, NewY). // nextStep zum Updaten neuer Position

+!updatePos(X, Y) : not last_step_not_OK <- -+pos(X, Y).  // Abfangbedingung

+!updatePos(X, Y) : last_step_not_OK
    <- -last_step_not_OK;
       .print("Benni last step not ok"). // Letzter Schritt war nicht erfolgreich
       !search_sheep. // Starte die Suche nach Schafen erneut

+!get_random_position(X, Y) <- jia.random_position(X, Y). 



