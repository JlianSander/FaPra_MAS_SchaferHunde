// Initialize the search for sheep
+!search_sheep(X,Y,R) <- 
    .print("Starting sheep search within radius ", R); 
    ?pos(AgX, AgY);
    !find_sheep(AgX, AgY, R).

// Find sheep within the given radius
+!find_sheep(AgX, AgY, R) <- 
    jia.get_nearest_sheep_in_range(AgX, AgY, X, Y, R); 
    .print("Found sheep flock at (", X, ",", Y, "). Broadcasting to other hounds.");
    .my_name(Me);
    .broadcast(tell, found_sheep(X, Y)).

// If no sheep found, continue the search
-find_sheep(AgX, AgY, R) <- 
    .print("No sheep flock in sight. Continuing search.");
    !move_random(AgX, AgY, R);
    !search_sheep(_, _, R).

// Move to a random position to continue the search
+!move_random(AgX, AgY, R) <- 
    randomStep(AgX, AgY, R, NewX, NewY);
    !walkTowards(NewX, NewY).

// Walk towards the given coordinates
+!walkTowards(X,Y) : not pos(X,Y) <- 
    .print("Walking towards: (",X,",",Y,")");
    !makeStepTowards(X,Y);
    .wait(100);
    !walkTowards(X,Y).

+!walkTowards(X,Y) <- 
    .print("Reached destination: (",X,",",Y,")").
