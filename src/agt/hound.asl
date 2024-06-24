//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////
jammed(0).

+!init : true
    <- .my_name(Me);
       .broadcast(tell, hound(Me));
       jia.get_corral_area(TLX,TLY,BRX,BRY);
       +corral_area(TLX,TLY,BRX,BRY);
       .print("corral is in the area of (",TLX, ",", TLY,")x(", BRX, ",", BRY, ")");
       !!perceiveSurrounding;
       !!startSearch;
       .print("Finished init hound").

//-!G[error(no_relevant), error_msg(Msg)] <- .print("ERROR: ", Msg).                //!!!!!!!!!!!!!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!! to silence error message
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

in_sight(X,Y) :- pos(AgX, AgY) & jia.in_line_of_sight(AgX, AgY, X, Y).

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
    !waitToMove;
    !walkTowards(X,Y).

+!walkTowards(X,Y) <- .print("walking finished").   //reached target coordinates
                                                                                                  
//------------------------------------------------------- makeStepTowards -------------------------------------------------------
+!makeStepTowards(X,Y) : pos(X,Y) <- true.

@step[atomic]
+!makeStepTowards(X,Y)<- 
    nextStep(X,Y, NewX, NewY);
    //.print("stepped to new position: (",NewX,",",NewY,")");
    !updatePos(NewX,NewY).       

-!makeStepTowards(X,Y) : is_jammed
    <- -+jammed(0);
    .print("end retrying");
    //+last_step_not_OK;
    false.

-!makeStepTowards(X,Y) <- .print("waiting (jammed)");                                                                                                    
    ?jammed(J);
    -+jammed(J + 1);
    //.wait({+mapChanged});
    !waitToMove;
    !makeStepTowards.     //retry making step 

//------------------------------------------------------- perceiveSurrounding -------------------------------------------------------

+!perceiveSurrounding 
    <- //.print("perceiveSurrounding");
    jia.look_around;
    ?wait_perception(W);
    .wait(W);
    !!perceiveSurrounding.

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("agent.asl") }
{ include("hound_drive.asl")}
{ include("hound_search.asl")}