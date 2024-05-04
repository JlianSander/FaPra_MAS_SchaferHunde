//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////
jammed(0).

+!init : true
    <- .my_name(Me);
       .broadcast(tell, hound(Me));
       jia.get_corral_area(TLX,TLY,BRX,BRY);
       +corral_area(TLX,TLY,BRX,BRY);
       .print("corral is in the area of (",TLX, ",", TLY,")x(", BRX, ",", BRY, ")").

//-!G[error(no_relevant), error_msg(Msg)] <- .print("ERROR: ", Msg).                //!!!!!!!!!!!!!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!! to silence error message
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

in_sight(X,Y) :- pos(AgX, AgY) & jia.in_line_of_sight(AgX, AgY, X, Y, 7).

is_jammed :- jammed(J) & J > 10.

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- reachDestination -------------------------------------------------------
 
+!reachDestination(X,Y) : .desire(reachDestination(L,M)) & (L \== X | M \== Y) 
    <- .print("drop intention of reaching (",L,",",M,")");
    .drop_desire(walkTowards(L,M));   //ensure only walking in one direction at the same time
    .print("start reaching :(",X,",",Y,")"); 
    !walkTowards(X,Y).

+!reachDestination(X,Y) <- .print("start reaching :(",X,",",Y,")"); 
    !walkTowards(X,Y).

//------------------------------------------------------- walkTowards -------------------------------------------------------

+!walkTowards(X,Y) : not pos(X,Y)   //not yet reached target coordinates
    <- .print("walking towards: (",X,",",Y,")");
    !makeStepTowards(X,Y);
    !walkTowards(X,Y).

+!walkTowards(X,Y) <- .print("reached destination").   //reached target coordinates
                                                                                                  
//------------------------------------------------------- makeStepTowards -------------------------------------------------------
@step[atomic]
+!makeStepTowards(X,Y)<- 
    nextStep(X,Y, NewX, NewY);
    .print("stepped to new position: (",NewX,",",NewY,")");
    !updatePos(NewX,NewY);
    //.wait(300).  
    !!wait(100).       

-!makeStepTowards(X,Y) : is_jammed
    <- -+jammed(0);
    .print("end retrying");
    +last_step_not_OK;
    false.

-!makeStepTowards(X,Y) <- .print("waiting (jammed)");                                                                                                    
    ?jammed(J);
    -+jammed(J + 1);
    .wait({+mapChanged});
    !makeStepTowards.     //retry making step 

//------------------------------------------------------- wait -------------------------------------------------------

+!wait(X) <- .wait(X).

//------------------------------------------------------- observe -------------------------------------------------------
@handle_new_sheep_fail[atomic]
+!handle_new_sheep(A) : .desire(driveTarget(_)) <- .print("I'm already driving a sheep."); .succeed_goal({handle_new_sheep(A)}).

@handle_new_sheep_target[atomic]
+!handle_new_sheep(A) <- .print("handle_new_sheep: ", A);                                                                                                       
    !!driveTarget(A). //TODO:  ersetzen durch Plan zum einschätzen der Lage, Hund sollte nicht direkt erst besten Schaf hinterher jagen / Ist Treiben noch sinnvoll? / Ist Treiben sinnvoll geworden?

//------------------------------------------------------- trackMove -------------------------------------------------------
+!trackMove(X, Y)[source(S)] : in_sight(X,Y) & sheep(S) //only observe sheep
    <- -+pos_agent(X ,Y)[source(S)];
    !handle_new_sheep(S).

+!trackMove(X, Y)[source(S)] : in_sight(X,Y) & hound(S)
    <- -+pos_agent(X ,Y)[source(S)]. 

+!trackMove(X,Y) <- true.
//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }
{ include("agent.asl") }
{ include("hound_drive.asl")}
{ include("hound_search.asl")}