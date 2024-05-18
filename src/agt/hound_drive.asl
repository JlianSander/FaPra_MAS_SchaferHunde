//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

has_enough_info(S) :- pos_agent(SX,SY)[source(S)] & corral_area(TLX,TLY,BRX,BRY).

is_in_corral(S) :- pos_agent(SX,SY)[source(S)] & jia.is_in_corral(SX, SY).

other_hound_is_closer(S) :- pos_agent(SX,SY)[source(S)] & 
    pos_agent(HX,HY)[source(H)] & hound(H) & jia.get_distance(SX,SY,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(SX,SY,ME_X,ME_Y,D_ME) &
    DH < D_ME.

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- driveSheep -------------------------------------------------------

+!driveSheep(S) : has_enough_info(S) & not is_in_corral(S) & not other_hound_is_closer(S) <- .print("driving: ", S); 
    ?pos_agent(SX,SY)[source(S)];
    jia.get_pos_drive_swarm(SX, SY, 0, ME_TARGET_X, ME_TARGET_Y);
    .print("Sheep is at (",SX,",",SY,") position agent at (",ME_TARGET_X, ",", ME_TARGET_Y, ")");
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y);
    !driveSheep(S).

+!driveSheep(S) : other_hound_is_closer(S) <- .print("Other hound is closer").

+!driveSheep(S) : not has_enough_info(S) <- .print("Not enough info to drive target").

+!driveSheep(S) : has_enough_info(S) & is_in_corral(S) <- .print("sheep is in corral").


//------------------------------------------------------- driveSwarm -------------------------------------------------------

+!driveSwarm(LS) : .desire(driveSwarmSingleton(_)) <- .print("I'm already driving a swarm."); .succeed_goal({handle_new_sheep(A)}).

+!driveSwarm(LS) <- driveSwarmSingleton(LS).

//------------------------------------------------------- driveSwarmSingleton -------------------------------------------------------

+!driveSwarmSingleton(LS) : 
    <- .length(LS, LS_length);
    +swarm_to_drive(LS, -1,-1, 0, -1);
    .print("Remembers swarm: ", LS);
    !processKnownSwarm(LS);
    .print("Forgot swarm: ", LS);
    -swarm_to_drive(_, _, _, _, _).

//------------------------------------------------------- processKnownSwarm -------------------------------------------------------

+!processKnownSwarm(LS)
    <- 
    // update swarm and create new list of current member and make recursive call with new list
    !updateSwarmCenter(LS);
    .wait(1000);
    !processKnownSwarm(LS).
    


//------------------------------------------------------- updateSwarmCenter -------------------------------------------------------
+!updateSwarmCenter([], LS).

+!updateSwarmCenter([S|Tail], LS)
    <- .print(S);
    ?pos_agent(SX,SY)[source(S)];
    ?swarm(LS, CX,CY, LS_length, R);
    if(LS_length == 0){
        -swarm_to_drive(_, _, _, _, _);
        +swarm_to_drive(LS, SX,SY, LS_length + 1, 0);
    }else{
        jia.get_new_swarm_data(CX, CY, LS_length, R, SX, SY, New_CX, New_CY, New_R);
        -swarm_to_drive(_, _, _, _, _);
        +swarm_to_drive(LS, New_CX,New_CY, LS_length + 1, New_R);
    }.