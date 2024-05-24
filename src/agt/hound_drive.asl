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

//------------------------------------------------------- driveSheep -------------------------------------------------------

+!driveSheep(S) : has_enough_info(S) & not is_in_corral(S) & not other_hound_is_closer_to_sheep(S) 
    <- .print("driving: ", S); 
    ?pos_agent(SX,SY)[source(S)];
    jia.get_pos_drive_swarm(SX, SY, 0, ME_TARGET_X, ME_TARGET_Y);
    .print("Sheep is at (",SX,",",SY,") position agent at (",ME_TARGET_X, ",", ME_TARGET_Y, ")");                                            //DEBUG
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y);
    !driveSheep(S).

+!driveSheep(S) : other_hound_is_closer_to_sheep(S) <- .print("Other hound is closer").

+!driveSheep(S) : not has_enough_info(S) <- .print("Not enough info to drive target").

+!driveSheep(S) : has_enough_info(S) & is_in_corral(S) <- .print("sheep is in corral").

//------------------------------------------------------- startDrive -------------------------------------------------------

+!startDrive : has_enough_info & not .desire(processDriving)
    <- !processDriving.

+!startDrive : not has_enough_info <- .print("Not enough info to drive."); .fail_goal(startDrive).

+!startDrive : .desire(processDriving) <- .print("Already started driving."); .fail_goal(startDrive).       

//------------------------------------------------------- processDriving -------------------------------------------------------

+!processDriving : has_enough_info & not (.desire(driveSwarm(_)) | .desire(mapSwarms) )  
    <- !mapSwarms;
    .findall(Swarm, swarm(Swarm,_,_,_,_),Swarms);
    .print("found swarms: ", Swarms);                                                                                                           //DEBUG
    if(.length(Swarms, 0)){
        //no swarm found
        .print("no swarm found");                                                                                                               //DEBUG
       .fail_goal(processDriving);
    }
    !chooseSwarmToDrive(Swarms);
    ?swarm_chosen_to_drive(Swarm_Chosen);
    !driveSwarm(Swarm_Chosen);
    .wait(50);   //DEBUG                                                                                                                    //DEBUG 
    !processDriving.    


//------------------------------------------------------- driveSwarm -------------------------------------------------------

+!driveSwarm(LS) 
    <- .print("driveSwarm(", LS, ")");                                                                                                      //DEBUG
    !updateSwarmData(LS);
    ?swarm_data_updated(LS, CX,CY, Size, R);
    jia.get_pos_drive_swarm(CX, CY, R, ME_TARGET_X, ME_TARGET_Y);
    //.print("Swarm is at (",CX,",",CY,") position agent at (", ME_TARGET_X, ",", ME_TARGET_Y, ")");                                          //DEBUG
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y).

//------------------------------------------------------- updateSwarmData -------------------------------------------------------

+!updateSwarmData(LS)
    <- //.print("updateSwarmData(",LS,")");                                                                                                     //DEBUG
    if(swarm_data_updated(_, _, _, _, _)){
        .abolish(swarm_data_updated(_,_,_,_,_));
    }
    +swarm_data_updated(LS, -1,-1, 0, -1);
    //.print("reset swarm: ", LS);                                                                                                            //DEBUG
    for(.member(S,LS)){
        //.print("updateSwarmData(", S, " of ", LS, ")");                                                                                     //DEBUG
        ?pos_agent(SX,SY)[source(S)];
        ?swarm_data_updated(LS, CX,CY, Size, R);
        jia.update_swarm_data(CX, CY, Size, R, SX, SY, New_CX, New_CY, New_Size, New_R);
        .abolish(swarm_data_updated(_,_,_,_,_));
        +swarm_data_updated(LS, New_CX,New_CY, New_Size, New_R);
        //.print("Swarm updated: Center (", New_CX, ",", New_CY, "); Size: ", New_Size, "; Radius: ", New_R );                                  //DEBUG
    }.

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("hound_drive_cluster_swarms.asl")}
{ include("hound_drive_choose_swarm.asl")}