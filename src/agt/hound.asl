//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////
jammed(0).

+!init : true
    <- .my_name(Me);
       .broadcast(tell, hound(Me));
       jia.get_corral_area(TLX,TLY,BRX,BRY);
       +corral_area(TLX,TLY,BRX,BRY);
       .print("corral is in the area of (",TLX, ",", TLY,")x(", BRX, ",", BRY, ")");
       .set.create(S);
       +ignoredSheep(S);
       !!perceiveSurrounding;
       !!startSearch;
       .print("Finished init hound").

//-!G[error(no_relevant), error_msg(Msg)] <- .print("ERROR: ", Msg).                //!!!!!!!!!!!!!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!! to silence error message
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    
i_am_lower_than(H):- .my_name(Me) & Me < H.

distance_between_agents(A1, A2, D):-  pos_agent(A1X,A1Y, A1) & pos_agent(A2X,A2Y, A2) & jia.get_distance(A1X,A1Y,A2X,A2Y,D).

distance_between_swarms_closest_members(Ss1, Ss2, Min_D):-  
            Ss1 \== Ss2 
            & swarm(Ss1, _, _, _)
            & swarm(Ss2, _, _, _)
            & .findall(D, .member(S1, Ss1) & .member(S2, Ss2) & S1 \== S2 & distance_between_agents(S1, S2, D), Distances)
            & .min(Distances, Min_D).

distance_between_swarms_farest_members(Ss1, Ss2, Max_D):-  
            Ss1 \== Ss2 
            & swarm(Ss1, _, _, _)
            & swarm(Ss2, _, _, _)
            & .findall(D, .member(S1, Ss1) & .member(S2, Ss2) & S1 \== S2 & distance_between_agents(S1, S2, D), Distances)
            & .max(Distances, Max_D).

distance_me_to_pos(X,Y, D_Me):- pos(Me_X, Me_Y)  & jia.get_distance(X,Y,Me_X,Me_Y,D_Me).

distance_me_to_swarm(Ss, D):- swarm(Ss, CX, CY, R) & pos(ME_X, ME_Y)  & jia.get_distance(CX,CY,ME_X,ME_Y,D).

distance_other_hound_to_pos(X,Y,H, DH):- 
    pos_agent(HX,HY, H) & hound(H) & jia.get_distance(X,Y,HX,HY,DH).

is_closer_to_pos(X,Y,H) :- 
    distance_other_hound_to_pos(X,Y,H, DH) & 
    distance_me_to_pos(X,Y, D_Me) &
    DH < D_Me.
    
is_equal_away_to_pos(X,Y,H) :- 
    distance_other_hound_to_pos(X,Y,H, DH) & 
    distance_me_to_pos(X,Y, D_Me) &
    DH = D_Me.

is_closer_to_swarm(H, Ss):- swarm(Ss, CX, CY, R) & 
    pos_agent(HX,HY, H) & hound(H) & jia.get_distance(CX,CY,HX,HY,DH) & 
    distance_me_to_swarm(Ss, D_ME) &
    DH < D_ME.

exists_close_swarms_single_linkage :- 
    .setof(Ss2, 
        swarm(Ss2, _, _, _) 
        & swarm(Ss3, _, _, _) 
        & Ss2 \== Ss3
        & swarms_are_close_to_eachother_single_linkage(Ss2, Ss3), CloseSwarms)
    & .length(CloseSwarms, CloseSwarms_Len) 
    & CloseSwarms_Len > 0.

exists_close_swarms_complete_linkage :- 
    .setof(Ss2, 
        swarm(Ss2, _, _, _) 
        & swarm(Ss3, _, _, _) 
        & Ss2 \== Ss3
        & swarms_are_close_to_eachother_complete_linkage(Ss2, Ss3), CloseSwarms)
    & .length(CloseSwarms, CloseSwarms_Len) 
    & CloseSwarms_Len > 0.

is_in_corral(S) :- pos_agent(SX,SY, S) & jia.is_in_corral(SX, SY).

is_jammed :- jammed(J) & limit_jammed_retries(N) & J > N.

+pos_agent(X,Y,S) : sheep(S) & .findall(S1, sheep(S1), Ss) & .length(Ss, Len_Ss) & Len_Ss > 3 <- !!startDrive.

swarms_are_close_to_eachother_single_linkage(Ss1, Ss2) :-  distance_between_swarms_closest_members(Ss1, Ss2, D) & cluster_swarm_limit_distance_member(Limit_distance) & D <= Limit_distance.

swarms_are_close_to_eachother_complete_linkage(Ss1, Ss2) :-  distance_between_swarms_farest_members(Ss1, Ss2, D) & cluster_swarm_limit_distance_member(Limit_distance) & D <= Limit_distance.
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

//------------------------------------------------------- ignoreSheep -------------------------------------------------------
+!ignoreSheep(LS)
    <- //.print("ignoreSheep(", LS, ")");
    ?ignoredSheep(IgnoredSheep);
    .set.add_all(IgnoredSheep, LS).
    //.print("ignoredSheep: ", IgnoredSheep).

//------------------------------------------------------- forgetIgnoreSheep -------------------------------------------------------
+!forgetIgnoreSheep(LS)
    <- //.print("forgetIgnoreSheep(", LS, ")");
    ?wait_ignore_sheep_forget(W);
    .wait(W);
    ?ignoredSheep(IgnoredSheep);
    .set.difference(IgnoredSheep,LS).
    //.print("ignoredSheep: ", IgnoredSheep).

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("agent.asl") }
{ include("hound_drive.asl")}
{ include("hound_search.asl")}