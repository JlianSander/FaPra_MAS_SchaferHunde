!wait_and_end_simulation(5000).

+!wait_and_end_simulation(X)
    <- .print("Ending simulation after ", X, " seconds");
        .wait(X);
        endSimulation;
        .stopMAS.

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }