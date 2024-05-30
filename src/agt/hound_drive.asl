+!init_drive <-
    !!processDriving.                               //DEBUG

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

has_enough_info(S) :- pos_agent(SX,SY)[source(S)] & corral_area(TLX,TLY,BRX,BRY).

has_enough_info :- corral_area(TLX,TLY,BRX,BRY).

is_in_corral(S) :- pos_agent(SX,SY)[source(S)] & jia.is_in_corral(SX, SY).

other_hound_is_closer_to_sheep(S) :- pos_agent(SX,SY)[source(S)] & 
    pos_agent(HX,HY)[source(H)] & hound(H) & jia.get_distance(SX,SY,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(SX,SY,ME_X,ME_Y,D_ME) &
    DH < D_ME.


//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- startDrive -------------------------------------------------------

+!startDrive : has_enough_info & not .desire(processDriving)
    <- !processDriving.

+!startDrive : not has_enough_info <- .print("Not enough info to drive."); false. //.fail_goal(startDrive).

+!startDrive : .desire(processDriving) <- .print("Already started driving."); false. //.fail_goal(startDrive).       

//------------------------------------------------------- processDriving -------------------------------------------------------

+!processDriving  
    <- !mapSwarms;
    .findall(Swarm, swarm(Swarm,_,_,_,_),Swarms);
    //.print("found swarms: ", Swarms);                                                                                                           //DEBUG
    if(.length(Swarms, 0)){
        //no swarm found
        .print("no swarm found");                                                                                                               //DEBUG
       .fail_goal(processDriving);
    }
    !chooseSwarmToDrive(Swarms);
    if(swarm_chosen_to_drive(_)){
        ?swarm_chosen_to_drive(Swarm_Chosen);
        .print("Swarm chosen to drive: ", Swarm_Chosen);
        !driveSwarm(Swarm_Chosen);
        .wait(50);
        !processDriving;
    }else{
        .print("no swarm chosen");
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
    //.print("Swarm is at (",CX,",",CY,") with R: ", R, "; Position agent in Pos ", Driving_Position, " at (", ME_TARGET_X, ",", ME_TARGET_Y, ")");                         //DEBUG
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y).

-!driveSwarm(LS) <- true.

//------------------------------------------------------- updateSwarmData -------------------------------------------------------

+!updateSwarmData(LS)
    <- //.print("updateSwarmData(",LS,")");                                                                                                 //DEBUG
    .findall(X, pos_agent(X,Y)[source(S)] & .member(S,LS), List_of_X);
    .findall(Y, pos_agent(X,Y)[source(S)] & .member(S,LS), List_of_Y);
    CX = math.round(math.mean(List_of_X));
    CY = math.round(math.mean(List_of_Y));
    .length(LS, Len_LS);
    .findall(R, pos_agent(X,Y)[source(S)] & .member(S,LS) & jia.get_distance(CX,CY,X,Y,R), List_of_R);   
    R = math.round(math.max(List_of_R));
    .abolish(swarm_data_updated(_,_,_,_,_));
    +swarm_data_updated(LS, CX, CY, Len_LS, R);
    //.print("Updated swarm: (", LS, ", ", CX, ", ", CY, ", ", Len_LS, ", ", R, ")");                                                       //DEBUG
    .

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("./hound_drive/hound_drive_cluster_swarms.asl")}
{ include("./hound_drive/hound_drive_choose_swarm.asl")}
{ include("./hound_drive/hound_drive_plan_position.asl")}