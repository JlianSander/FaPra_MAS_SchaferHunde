//In diesem Code wird die interne Aktion findNearestSheep verwendet, 
//um das nächstgelegene Schaf zu suchen. Die Koordinaten des gefundenen Schafs werden dann als Ziel gesetzt, 
//und der Hound-Agent initiiert einen Zug zum Schaf durch Senden einer entsprechenden Nachricht.

////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////

// Keine spezielle Initialisierung erforderlich

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

// Keine speziellen Überzeugungen erforderlich

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- searchSheep -------------------------------------------------------
/*
+flock_target(X,Y) <- .print("Starting sheep search."); 
    ?pos(AgX, AgY);
    .jia.findNearestSheep(AgX, AgY, X, Y);
    .print("Found nearest sheep at (", X, ",", Y, ")");
    +destination(X,Y);
    .my_name(Me);
    .broadcast(achieve, driveTarget(Me));
    .print("Initiating drive towards sheep").
*/
//