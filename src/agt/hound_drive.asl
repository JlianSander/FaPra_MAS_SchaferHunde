//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

has_enough_info(S) :- pos_agent(SX,SY, S) & corral_area(TLX,TLY,BRX,BRY).

has_enough_info :- corral_area(TLX,TLY,BRX,BRY).

has_sheep_in_sight :- pos(Xme, Yme) & pos_agent(XS, YS, S) & sheep(S) & jia.common.in_line_of_sight(Xme, Yme, XS, YS).

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- startDrive -------------------------------------------------------
@startDrive[atomic]
+!startDrive : has_enough_info & not is_driving
    <- .print("startDrive");
    +is_driving;
    !!endDrive. 
    
+!startDrive : not has_enough_info <- .print("Not enough info to drive."). //.fail_goal(startDrive).

+!startDrive : is_driving <- true. //.print("Already started driving."). //.fail_goal(startDrive).       

//------------------------------------------------------- endDrive -------------------------------------------------------

+!endDrive 
    <- !processDriving.

-!endDrive
    <- .print("endDrive");
    -is_driving;
    !!startSearch.

//------------------------------------------------------- processDriving -------------------------------------------------------

+!processDriving : not has_sheep_in_sight
    <- .fail_goal(processDriving);
    .

+!processDriving : has_sheep_in_sight 
    <- .print("processDriving");                                                                                                                //DEBUG
    !clusterSwarms;
    .findall(Swarm, swarm(Swarm,_,_,_),Swarms);
    //.print("found swarms: ", Swarms);                                                                                                         //DEBUG
    if(.length(Swarms, 0)){
        //no swarm found
        .print("no swarm found");                                                                                                               //DEBUG
        .fail_goal(processDriving);
    }
    !selectSwarmToDrive(Swarms);
    if(swarm_chosen_to_drive(_)){
        ?swarm_chosen_to_drive(Swarm_Chosen);
        .print("Swarm chosen to drive: ", Swarm_Chosen);
        !driveSwarm(Swarm_Chosen);
        ?wait_between_driving(Wait_between_driving);
        .wait(Wait_between_driving);
        !processDriving;
    }else{
        .print("no swarm chosen");
        .setof(S, sheep(S) & pos_agent(_,_,S), LS);
        !ignoreSheep(LS);
        !!forgetIgnoreSheep(LS);
        .fail_goal(processDriving);
    }.    

//------------------------------------------------------- driveSwarm -------------------------------------------------------

+!driveSwarm(LS) 
    <- //.print("driveSwarm(", LS, ")");                                                                                                                                    //DEBUG
    !updateSwarmData(LS);
    ?swarm(LS, CX, CY, R);
    !planPositionToDrive(LS);
    ?driving_position(Driving_Position);
    jia.hounds.get_pos_drive_swarm(CX, CY, R, Driving_Position, ME_TARGET_X, ME_TARGET_Y);
    .print("Swarm is at (",CX,",",CY,") with R: ", R, "; Position agent in Pos ", Driving_Position, " at (", ME_TARGET_X, ",", ME_TARGET_Y, ")");                           //DEBUG
    ?pos(ME_X, ME_Y);
    jia.hounds.get_evasion_directions(CX, CY, R, EVADE_X, EVADE_Y);
    //.print("My Pos: ", ME_X, ",", ME_Y, " Target Pos: ", ME_TARGET_X, ",", ME_TARGET_Y );                                                                                   //DEBUG
    jia.hounds.get_next_pos(ME_X, ME_Y, EVADE_X, EVADE_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    .print("My Pos: ", ME_X, ",", ME_Y, " Target Pos: ", ME_TARGET_X, ",", ME_TARGET_Y , ", Next Step to Pos ", ME_NXT_X, ",", ME_NXT_Y);                                   //DEBUG
    if(ME_X == ME_NXT_X & ME_Y == ME_NXT_Y){
        //can't reach desired target 
        //TODO hier Zähler hochzählen und ab Grenzwert Plan B starten (zurückweichen oder Herde sprengen)
        !processStayingOnSamePos(LS);
    }else{
        .abolish(same_pos(_));
        !reachDestination(ME_NXT_X, ME_NXT_Y);
    }.

//------------------------------------------------------- processStayingOnSamePos -------------------------------------------------------

+!processStayingOnSamePos(LS) : same_pos(I) & limit_jammed_retries(N) & I > N
    <-  .print("processStayingOnSamePos !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! abort Drive");                                                                                                                             //DEBUG 
    !ignoreSheep(LS);
    !!forgetIgnoreSheep(LS);
    .abolish(same_pos(_));
    .print("fail processDriving");
    .fail_goal(processDriving).

+!processStayingOnSamePos(LS) : same_pos(I) & limit_jammed_retries(N) & I <= N
    <- .print("processStayingOnSamePos ", I);                                                                                                                               //DEBUG   
    -+same_pos(I + 1);
    ?wait_cant_reach_driving_pos(W);
    .wait(W).

+!processStayingOnSamePos(LS) : not same_pos(_) 
    <- .print("processStayingOnSamePos 0");                                                                                                                               //DEBUG   
    +same_pos(1);
    ?wait_cant_reach_driving_pos(W);
    .wait(W).

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("./hound_drive/hound_drive_cluster_swarms.asl")}
{ include("./hound_drive/hound_drive_select_swarm.asl")}
{ include("./hound_drive/hound_drive_plan_position.asl")}
{ include("./hound_drive/hound_drive_misc_plans.asl")}