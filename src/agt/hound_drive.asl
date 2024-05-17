//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

has_enough_info(S) :- pos_agent(SX,SY)[source(S)] & corral_area(TLX,TLY,BRX,BRY).

is_in_corral(S) :- pos_agent(SX,SY)[source(S)] & jia.is_in_corral(SX, SY).

other_hound_is_closer(S) :- pos_agent(SX,SY)[source(S)] & 
    pos_agent(HX,HY)[source(H)] & hound(H) & jia.get_distance(SX,SY,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(SX,SY,ME_X,ME_Y,D_ME) &
    DH < D_ME.

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- driveTarget -------------------------------------------------------

+!driveTarget(S) : has_enough_info(S) & not is_in_corral(S) & not other_hound_is_closer(S) <- .print("driving: ", S); 
    ?pos_agent(SX,SY)[source(S)];
    jia.get_pos_drive_swarm(SX, SY, 0, ME_TARGET_X, ME_TARGET_Y);
    .print("Sheep is at (",SX,",",SY,") position agent at (",ME_TARGET_X, ",", ME_TARGET_Y, ")");
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y);
    !driveTarget(S).

+!driveTarget(S) : other_hound_is_closer(S) <- .print("Other hound is closer").

+!driveTarget(S) : not has_enough_info(S) <- .print("Not enough info to drive target").

+!driveTarget(S) : has_enough_info(S) & is_in_corral(S) <- .print("sheep is in corral").


//------------------------------------------------------- driveSwarm -------------------------------------------------------

+!driveSwarm(LS) <- 
    +swarm(LS);
    .print("Remembers swarm: ", LS);
    .wait(1000);
    // update swarm and create new list of current member and make recursive call with new list
    .print("Forgot swarm: ", LS);
    -swarm(LS).
    