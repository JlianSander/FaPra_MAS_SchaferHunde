+!init_drive <-
    !!processDriving.                               //DEBUG

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

has_enough_info(S) :- pos_agent(SX,SY, S) & corral_area(TLX,TLY,BRX,BRY).

has_enough_info :- corral_area(TLX,TLY,BRX,BRY).

is_in_corral(S) :- pos_agent(SX,SY, S) & jia.is_in_corral(SX, SY).

other_hound_is_closer_to_sheep(S) :- pos_agent(SX,SY, S) & 
    pos_agent(HX,HY, H) & hound(H) & jia.get_distance(SX,SY,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(SX,SY,ME_X,ME_Y,D_ME) &
    DH < D_ME.


//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- startDrive -------------------------------------------------------

+!startDrive : has_enough_info & not .desire(processDriving)
    <- !processDriving.

+!startDrive : not has_enough_info <- .print("Not enough info to drive."). //.fail_goal(startDrive).

+!startDrive : .desire(processDriving) <- .print("Already started driving."). //.fail_goal(startDrive).       

//------------------------------------------------------- processDriving -------------------------------------------------------

+!processDriving  
    <- !mapSwarms;
    .findall(Swarm, swarm(Swarm,_,_,_,_),Swarms);
    //.print("found swarms: ", Swarms);                                                                                                           //DEBUG
    if(.length(Swarms, 0)){
        //no swarm found
        .print("no swarm found");                                                                                                               //DEBUG
        //!!search_sheep;
        .fail_goal(processDriving);
    }
    !chooseSwarmToDrive(Swarms);
    if(swarm_chosen_to_drive(_)){
        ?swarm_chosen_to_drive(Swarm_Chosen);
        .print("Swarm chosen to drive: ", Swarm_Chosen);
        !driveSwarm(Swarm_Chosen);
        ?wait_between_driving(Wait_between_driving);
        .wait(Wait_between_driving);
        !processDriving;
    }else{
        .print("no swarm chosen");
        //!!search_sheep;
        .fail_goal(processDriving);
    }.    

//TODO wenn processDriving failed dann starte Suchen  (z.B. weil no swarm found)

//------------------------------------------------------- driveSwarm -------------------------------------------------------

+!driveSwarm(LS) 
    <- //.print("driveSwarm(", LS, ")");                                                                                                      //DEBUG
    !updateSwarmData(LS);
    ?swarm_data_updated(LS, CX,CY, Size, R);
    !planPositionToDrive(LS);
    ?driving_position(Driving_Position);
    jia.get_pos_drive_swarm(CX, CY, R, Driving_Position, ME_TARGET_X, ME_TARGET_Y);
    .print("Swarm is at (",CX,",",CY,") with R: ", R, "; Position agent in Pos ", Driving_Position, " at (", ME_TARGET_X, ",", ME_TARGET_Y, ")");                         //DEBUG
    ?pos(ME_X, ME_Y);
    ?keep_distance_to_swarm(Spacing);
    //.print("My Pos: ", ME_X, ",", ME_Y, " Target Pos: ", ME_TARGET_X, ",", ME_TARGET_Y , " Keep distance to herd: ", Spacing);                                              //DEBUG
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, Spacing, 0, ME_NXT_X, ME_NXT_Y);
    //.print("My Pos: ", ME_X, ",", ME_Y, " Target Pos: ", ME_TARGET_X, ",", ME_TARGET_Y , " Keep distance to herd: ", Spacing, " Next Step to Pos ", ME_NXT_X, ",", ME_NXT_Y);             //DEBUG
    if(ME_X == ME_NXT_X & ME_Y == ME_NXT_Y){
        //can't reach desired target 
        //TODO hier Zähler hochzählen und ab Grenzwert Plan B starten (zurückweichen oder Herde sprengen)
        .print("Can't reach target position.");                                                                                                                                 //DEBUG
        .wait(500); //DEBUG
    }else{
        !reachDestination(ME_NXT_X, ME_NXT_Y);
    }.    

-!driveSwarm(LS) <- true.

//------------------------------------------------------- updateSwarmData -------------------------------------------------------

+!updateSwarmData(LS)
    <- //.print("updateSwarmData(",LS,")");                                                                                                 //DEBUG
    .findall(X, pos_agent(X, Y, S) & .member(S,LS), List_of_X);
    .findall(Y, pos_agent(X, Y, S) & .member(S,LS), List_of_Y);
    CX = math.round(math.mean(List_of_X));
    CY = math.round(math.mean(List_of_Y));
    .length(LS, Len_LS);
    .findall(R, pos_agent(X,Y, S) & .member(S,LS) & jia.get_distance(CX,CY,X,Y,R), List_of_R);   
    R = math.round(math.max(List_of_R));
    .abolish(swarm_data_updated(_,_,_,_,_));
    +swarm_data_updated(LS, CX, CY, Len_LS, R);
    //.print("Updated swarm: (", LS, ", ", CX, ", ", CY, ", ", Len_LS, ", ", R, ")");                                                       //DEBUG
    .

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("./hound_drive/hound_drive_cluster_swarms.asl")}
{ include("./hound_drive/hound_drive_choose_swarm.asl")}
{ include("./hound_drive/hound_drive_plan_position.asl")}