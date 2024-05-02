//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////
jammed(0).

+!init : true
    <- .my_name(Me);
       .broadcast(tell, hound(Me)).

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    



//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////



//------------------------------------------------------- reachDestination -------------------------------------------------------

+!reachDestination(X,Y) : isRunning_reachDestination <- false. //already existing as intention -- can only walk to one destination at a time
    
+!reachDestination(X,Y) <- +isRunning_reachDestination;
    !walkTowards(X,Y);
    -isRunning_reachDestination.

-!reachDestination(X,Y) : isRunning_reachDestination <- -isRunning_reachDestination.

//------------------------------------------------------- walkTowards -------------------------------------------------------

+!walkTowards(X,Y) : not pos(X,Y)   //not yet reached target coordinates
    <- !makeStepTowards(X,Y);
    !walkTowards(X,Y).

+!walkTowards(X,Y) <- true;   //reached target coordinates
    .print("reached destination").                                                                                                //DEBUG

//------------------------------------------------------- makeStepTowards -------------------------------------------------------
+!makeStepTowards(X,Y)<- 
    nextStep(X,Y, NewX, NewY);
    -+pos(NewX, NewY);
    .wait(100).         

-!makeStepTowards(X,Y) : jammed(J) & //exception plan if move fails
    J > 10
    <- -+jammed(0);
    .print("end retrying");
    false.

-!makeStepTowards(X,Y) : jammed(J) & //exception plan if move fails
    J <= 10
    <- .print("waiting (jammed)");                                                                                                //DEBUG    
    -+jammed(J + 1);
    .wait(100);
    !makeStepTowards.     //retry making step 

//------------------------------------------------------- observe -------------------------------------------------------
+!observe(A, X, Y) : target(_)
    <- -+pos_agent(X ,Y)[source(A)].

+!observe(A, X, Y) : not target(_) 
    <- +target(A);
    -+pos_agent(X ,Y)[source(A)];
    .print("target: ", A);                                                                                                       //DEBUG
    !driveTarget.
//------------------------------------------------------- trackMove -------------------------------------------------------
+!trackMove(X, Y)[source(S)] : pos(AgX, AgY) & 
    jia.in_line_of_sight(AgX, AgY, X, Y, 7) &
    sheep(S) //only observe sheep
    <- !observe(S, X, Y).

+!trackMove(X,Y) <- true.
//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }
{ include("agent.asl") }
{ include("hound_drive.asl")}