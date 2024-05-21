+!search_sheep(X,Y,R) <- .print("Starting sheep search within radius ", R); 
                        ?pos(AgX, AgY);
                        jia.get_nearest_sheep_in_range(AgX, AgY, X, Y, R); // finde nächstes Schaf
                        .my_name(Me);
                        .broadcast(achieve, driveTarget(Me));
                        .print("Initiating drive towards sheep flock ", X, ",", Y).

+!search_sheep <- .print("No sheep flock in sight. Continuing search.").

+flock_target(X,Y,NbS,R) <- .print("Starting sheep chase towards flock at (", X, ",", Y, ").");
                             +destination(X,Y);
                             .my_name(Me);
                             .broadcast(achieve, driveTarget(Me));
                             .print("Initiating chase towards sheep flock at (", X, ",", Y, ").").

+flock_target(X,Y,NbS,R) <- .print("Flock of sheep at (", X, ",", Y, ") moved out of range. Continuing search.");
                             !search_sheep.

+!search_sheep[source(self)] <- .print("Starting sheep search.");
                                 !search_sheep.
