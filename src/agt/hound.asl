//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////

+!init : true
    <- .my_name(Me);
       .broadcast(tell, hound(Me));
       jia.hounds.get_corral_area(TLX,TLY,BRX,BRY);
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

distance_between_agents(A1, A2, D):-  pos_agent(A1X,A1Y, A1) & pos_agent(A2X,A2Y, A2) & jia.common.get_distance(A1X,A1Y,A2X,A2Y,D).

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

distance_me_to_pos(X,Y, D_Me):- pos(Me_X, Me_Y)  & jia.common.get_distance(X,Y,Me_X,Me_Y,D_Me).

distance_me_to_swarm(Ss, D):- swarm(Ss, CX, CY, R) & pos(ME_X, ME_Y)  & jia.common.get_distance(CX,CY,ME_X,ME_Y,D).

distance_other_hound_to_swarm(Ss, H, D):- swarm(Ss, CX, CY, R) & pos_agent(XH, YH, H)  & jia.common.get_distance(CX, CY, XH, YH,D).

distance_other_hound_to_pos(X,Y,H, DH):- 
    pos_agent(HX,HY, H) & hound(H) & jia.common.get_distance(X,Y,HX,HY,DH).

is_closer_to_pos(X,Y,H) :- 
    distance_other_hound_to_pos(X,Y,H, DH) & 
    distance_me_to_pos(X,Y, D_Me) &
    DH < D_Me.
    
is_equal_away_to_pos(X,Y,H) :- 
    distance_other_hound_to_pos(X,Y,H, DH) & 
    distance_me_to_pos(X,Y, D_Me) &
    DH = D_Me.

is_closer_to_swarm(H, Ss):- swarm(Ss, CX, CY, R) & 
    pos_agent(HX,HY, H) & hound(H) & jia.common.get_distance(CX,CY,HX,HY,DH) & 
    distance_me_to_swarm(Ss, D_ME) &
    DH < D_ME.

is_H2_closer_to_swarm(H1, H2, Ss):-
    pos_agent(H2X,H2Y, H2) & distance_other_hound_to_swarm(Ss, H1, D1)
    & pos_agent(H1X,H1Y, H1) & distance_other_hound_to_swarm(Ss, H2, D2)
    & distance_me_to_swarm(Ss, D_ME) 
    & D2 < D1.

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

is_in_corral(S) :- pos_agent(SX,SY, S) & jia.common.is_in_corral(SX, SY).

swarms_are_close_to_eachother_single_linkage(Ss1, Ss2) :-  distance_between_swarms_closest_members(Ss1, Ss2, D) & cluster_swarm_limit_distance_member(Limit_distance) & D <= Limit_distance.

swarms_are_close_to_eachother_complete_linkage(Ss1, Ss2) :- swarm(Ss1, _, _, R1) 
        & swarm(Ss2, _, _, R2)
        &  distance_between_swarms_farest_members(Ss1, Ss2, D) & cluster_swarm_limit_distance_member(Limit_distance) & (D <= R1 + Limit_distance | D <= R2 + Limit_distance).
//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- reachDestination -------------------------------------------------------
 
+!reachDestination(X,Y) : .desire(reachDestination(L,M)) & (L \== X | M \== Y) 
    <- .print("can't reach (", X, ",", Y, ") since I'm reaching (",L,",",M,")");
    !waitToMove. 

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
+!makeStepTowards(X,Y) : pos(X,Y) 
    <- true.

@step[atomic]
+!makeStepTowards(X,Y)
    <- 
    .print("makeStepTowards(", X, ", ", Y, ")");
    nextStep(X,Y, NewX, NewY);
    .print("stepped to new position: (",NewX,",",NewY,")");
    !updatePos(NewX,NewY).

//------------------------------------------------------- perceiveSurrounding -------------------------------------------------------

+!perceiveSurrounding 
    <- //.print("perceiveSurrounding");
    jia.common.look_around;
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