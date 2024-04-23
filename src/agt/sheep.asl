!start.

+!start : true
    <- place_sheep;
       .print("Sheep initialized.");
       !moveRightContinuously.

+!moveRightContinuously
    <- test;
       .wait(100);
       !moveRightContinuously.

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }