!begin_simulation.

+!begin_simulation : timeout(T)
    <- startSimulation;
        !wait_and_end_simulation(T).

+!wait_and_end_simulation(T)
    <- .wait({+simulationEnded}, T, E);
        endSimulation;
        .stopMAS;
        .

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }