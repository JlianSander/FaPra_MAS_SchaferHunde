+!init_drive <-
    +limit_radius_swarm(7).


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

+!prepareToDrive : .desire(driveSwarmSingleton(_)) <- .print("I'm already driving a swarm."); .succeed_goal({driveSwarm}).

+!prepareToDrive <- 
    !mapSwarms;
    ?known_swarms(Swarms);
    //TODO: if no swarm found .succeed_goal({prepareToDrive})
    //TODO: choose most desirable swarm
    /* .setof(S, pos_agent(_,_)[source(S)], L);                                                                                                 //DEBUG
    .sort(L, Sorted_L);                                                                                                                      //DEBUG
    .print("Sheep I know:", Sorted_L);                                                                                                       //DEBUG
    !driveSwarmSingleton(Sorted_L); */
    .nth(0,Swarms,Swarm_Chosen);
    -known_swarms(_);
    !driveSwarmSingleton(Swarm_Chosen);
    .wait(100);   //DEBUG                                                                                                                    //DEBUG 
    !!prepareToDrive.                

//------------------------------------------------------- mapSwarms -------------------------------------------------------

+!mapSwarms <-
    .print("mapSwarms");
    //get all known sheep, which are not in the corral, in one big swarm
    .setof(S, pos_agent(_,_)[source(S)] & not is_in_corral(S) , All_Sheep);
    .print("All Sheeps not in Corral are: ", All_Sheep);
    !mapSetToSwarms(All_Sheep).
    

+!mapSetToSwarms(UnMapped_Set) <-
    .print("mapSetToSwarms for ", UnMapped_Set);
    //only calculate swarm for more than one sheep in the set
    if(.length(UnMapped_Set) > 1){
        //calculate center of swarm
        !updateSwarmDataStart(UnMapped_Set);
        ?swarm_to_drive(UnMapped_Set, CX,CY, Size, R);
        //check if swarm is cohesive enough
        ?limit_radius_swarm(Limit_rad);
        .print("R: ", R, " Limit_rad: ", Limit_rad);
        if(R < Limit_rad){
            !addSwarmToBB(UnMapped_Set);
        }else{
            //get all sheep outside the limit for the radius
            .setof(S, pos_agent(SX,SY)[source(S)] & .get_distance(SX,SY,CX,CY,D) & D > Limit_rad, Outsiders);
            .difference(UnMapped_Set, Outsiders, New_Swarm);
            .print("Of ", UnMapped_Set, " only ", New_Swarm, " are within the limit of ", Limit_rad, " tiles to the center: (", CX, ",", CY, ")");
            !addSwarmToBB(New_Swarm);
            !mapSetToSwarms(Outsiders);
        }
    } else{
        .print("Set ", UnMapped_Set, "has only one element.");
    }.


+!addSwarmToBB(NewSwarm) <-
    .print("addSwarmToBB for ", NewSwarm);
    if(known_swarms(Set_Swarms)){
        .print("add NewSwarm: ", NewSwarm, " to Set_Swarms: ", Set_Swarms);
        .set.add(Set_Swarms, NewSwarm);
    }else{
        +known_swarms(.set.create(Set_Swarms, NewSwarm));
        .print("Created new Set_Swarms: ", Set_Swarms);
    }.
//------------------------------------------------------- driveSwarmSingleton -------------------------------------------------------

+!driveSwarmSingleton(LS) 
    <- .print("Start driveSwarmSingleton");                                                                                                 //DEBUG
    !updateSwarmDataStart(LS);
    .print("Remembers swarm: ", LS);                                                                                                        //DEBUG
    ?swarm_to_drive(LS, CX,CY, Size, R);
    jia.get_pos_drive_swarm(CX, CY, R, ME_TARGET_X, ME_TARGET_Y);
    .print("Swarm is at (",CX,",",CY,") position agent at (", ME_TARGET_X, ",", ME_TARGET_Y, ")");                                          //DEBUG
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y);                                                                                                  //DEBUG
    -swarm_to_drive(_, _, _, _, _);
    .print("Forgot swarm: ", LS).

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