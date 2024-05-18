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
    .print("Sheep is at (",SX,",",SY,") position agent at (",ME_TARGET_X, ",", ME_TARGET_Y, ")");                                            //DEBUG
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y);
    !driveSheep(S).

+!driveSheep(S) : other_hound_is_closer(S) <- .print("Other hound is closer").

+!driveSheep(S) : not has_enough_info(S) <- .print("Not enough info to drive target").

+!driveSheep(S) : has_enough_info(S) & is_in_corral(S) <- .print("sheep is in corral").


//------------------------------------------------------- driveSwarm -------------------------------------------------------

+!driveSwarm : .desire(driveSwarmSingleton(_)) <- .print("I'm already driving a swarm."); .succeed_goal({handle_new_sheep(A)}).

+!driveSwarm <- 
    //TODO: map sheep to swarms (use beliefs about pos of sheep)
    //TODO: choose most desirable swarm
    .setof(S, pos_agent(_,_)[source(S)], L);                                                                                                 //DEBUG
    .sort(L, Sorted_L);                                                                                                                      //DEBUG
    .print("Sheep I know:", Sorted_L);                                                                                                       //DEBUG
    !driveSwarmSingleton(Sorted_L).
    //TODO: if(swarm reached corral){ true} else { !!driveSwarm}.

//------------------------------------------------------- driveSwarmSingleton -------------------------------------------------------

+!driveSwarmSingleton(LS) 
    <- .print("Start driveSwarmSingleton");                                                                                                 //DEBUG
    !updateSwarmDataStart(LS);
    .print("Remembers swarm: ", LS);                                                                                                        //DEBUG
    //TODO: drive this swarm for one round, next round evaluate anew which swarm to drive
    .print("Forgot swarm: ", LS);                                                                                                           //DEBUG
    -swarm_to_drive(_, _, _, _, _).

//------------------------------------------------------- updateSwarmDataStart -------------------------------------------------------

+!updateSwarmDataStart(LS)
    <- 
    if(swarm_to_drive(_, _, _, _, _)){
        -swarm_to_drive(_, _, _, _, _);
    }
    +swarm_to_drive(LS, -1,-1, 0, -1);
    .print("reset swarm: ", LS);                                                                                                            //DEBUG
    !updateSwarmData(LS, LS).

//------------------------------------------------------- updateSwarmData -------------------------------------------------------
+!updateSwarmData([], LS).

+!updateSwarmData([S|Tail], LS)
    <- .print("handle element: ", S, " in list: ", LS);                                                                                      //DEBUG
    ?pos_agent(SX,SY)[source(S)];
    ?swarm_to_drive(LS, CX,CY, Size, R);
    jia.update_swarm_data(CX, CY, Size, R, SX, SY, New_CX, New_CY, New_Size, New_R);
    -swarm_to_drive(_, _, _, _, _);
    +swarm_to_drive(LS, New_CX,New_CY, New_Size, New_R);
    .print("Swarm updated: Center (", New_CX, ",", New_CY, "); Size: ", New_Size, "; Radius: ", New_R );                                     //DEBUG
    !updateSwarmData(Tail, LS).