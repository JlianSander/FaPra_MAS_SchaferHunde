+!init_drive <-
    +limit_radius_swarm(5);
    !!processDriving.                               //DEBUG


//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

has_enough_info(S) :- pos_agent(SX,SY)[source(S)] & corral_area(TLX,TLY,BRX,BRY).
has_enough_info :- corral_area(TLX,TLY,BRX,BRY).

is_in_corral(S) :- pos_agent(SX,SY)[source(S)] & jia.is_in_corral(SX, SY).

other_hound_is_closer(S) :- pos_agent(SX,SY)[source(S)] & 
    pos_agent(HX,HY)[source(H)] & hound(H) & jia.get_distance(SX,SY,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(SX,SY,ME_X,ME_Y,D_ME) &
    DH < D_ME.


//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- driveSheep -------------------------------------------------------

+!driveSheep(S) : has_enough_info(S) & not is_in_corral(S) & not other_hound_is_closer(S) 
    <- .print("driving: ", S); 
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

//------------------------------------------------------- startDrive -------------------------------------------------------

+!startDrive : has_enough_info & not .desire(processDriving)
    <- !processDriving.

+!startDrive : not has_enough_info <- .print("Not enough info to drive."); .fail_goal(startDrive).

+!startDrive : .desire(processDriving) <- .print("Already started driving."); .fail_goal(startDrive).       

//------------------------------------------------------- processDriving -------------------------------------------------------

+!processDriving : has_enough_info & not (.desire(driveSwarm(_)) | .desire(mapSwarms) )  
    <- !mapSwarms;
    .findall(Swarm, known_swarms(Swarm),Swarms);
    .print("found swarms: ", Swarms);                                                                                                           //DEBUG
    if(.length(Swarms, 0)){
        //no swarm found
        .print("no swarm found");                                                                                                               //DEBUG
       .fail_goal(processDriving);
    }
    .nth(0,Swarms,Swarm_Chosen);
    //TODO: choose most desirable swarm
    .abolish(known_swarms(_));
    !driveSwarm(Swarm_Chosen);
    .wait(50);   //DEBUG                                                                                                                    //DEBUG 
    !processDriving.    

//------------------------------------------------------- mapSwarms -------------------------------------------------------

+!mapSwarms 
    <- //.print("mapSwarms");                                                                                                                  //DEBUG
    //get all known sheep, which are not in the corral, in one big swarm
    .setof(S, pos_agent(_,_)[source(S)] & not is_in_corral(S) , All_Sheep);
    if(.length(All_Sheep, 0)){
        //no swarm found
        .print("no sheep found");                                                                                                            //DEBUG
        .fail_goal(mapSwarms);
    }
    //.print("All Sheeps not in Corral are: ", All_Sheep);                                                                                    //DEBUG
    !mapSetToSwarms(All_Sheep).

//------------------------------------------------------- mapSetToSwarms ------------------------------------------------------- 

+!mapSetToSwarms(UnMapped_Set) 
    <- //.print("mapSetToSwarms(", UnMapped_Set, ")");                                                                                      //DEBUG
    //only calculate swarm for more than one sheep in the set
    if(.length(UnMapped_Set) > 1){
        !createSwarmWithoutOutsiders(UnMapped_Set);
        ?swarm_in_focus(New_Swarm, CX,CY, Size, R);
        +known_swarms(New_Swarm);
        .abolish(swarm_in_focus(_,_,_,_,_));
        
        if(outsiders(Outsiders)){
            -outsiders(_);
            !mapSetToSwarms(Outsiders);
        }
    } else{
        //.print("Set ", UnMapped_Set, " has only one element.");                                                                           //DEBUG
        +known_swarms(UnMapped_Set);
    }.

//------------------------------------------------------- createSwarmWithoutOutsiders ------------------------------------------------------- 

    +!createSwarmWithoutOutsiders(Set_Sheep)
        <- //.print("createSwarmWithoutOutsiders(", Set_Sheep, ")");                                                                      //DEBUG
        ?limit_radius_swarm(Limit_rad);
        for(.member(S, Set_Sheep)){
            //.print("createSwarmWithoutOutsiders(", S, " of ", Set_Sheep, ")");                                                          //DEBUG
            ?pos_agent(SX,SY)[source(S)];
            if(swarm_in_focus(_, _, _, _, _)){
                ?swarm_in_focus(LS, CX,CY, Size, R);
                //.print("swarm_in_focus:",LS,CX,CY, Size, R);                                                                            //DEBUG
                if(jia.get_distance(SX,SY,CX,CY,D) & D > Limit_rad){
                    if(outsiders(Outsiders)){
                        .set.add(Outsiders, S);
                    }else{
                        .set.create(New_Outsiders);
                        .set.add(New_Outsiders, S);
                        +outsiders(New_Outsiders);
                    }
                }else{
                    if(not .member(S, LS)){
                        // shouldn't happen.. but it does: Bug?
                        .print("LS: ", LS);
                        .print("S: ", S);
                        .set.add(LS,S);
                        !updateSwarmData(LS);
                    }
                }
            }else{
                .set.create(New_Swarm);
                .set.add(New_Swarm, S);
                !updateSwarmData(New_Swarm);
            }
        }.
//------------------------------------------------------- driveSwarm -------------------------------------------------------

+!driveSwarm(LS) 
    <- .print("driveSwarm(", LS, ")");                                                                                                      //DEBUG
    !updateSwarmData(LS);
    //.print("Remembers swarm: ", LS);                                                                                                      //DEBUG
    ?swarm_in_focus(LS, CX,CY, Size, R);
    jia.get_pos_drive_swarm(CX, CY, R, ME_TARGET_X, ME_TARGET_Y);
    .print("Swarm is at (",CX,",",CY,") position agent at (", ME_TARGET_X, ",", ME_TARGET_Y, ")");                                          //DEBUG
    ?pos(ME_X, ME_Y);
    jia.get_next_pos(ME_X, ME_Y, ME_TARGET_X, ME_TARGET_Y, ME_NXT_X, ME_NXT_Y);
    !reachDestination(ME_NXT_X, ME_NXT_Y);
    .abolish(swarm_in_focus(_,_,_,_,_)).
    //.print("Forgot swarm: ", LS).                                                                                                          //DEBUG

//------------------------------------------------------- updateSwarmData -------------------------------------------------------

+!updateSwarmData(LS)
    <- //.print("updateSwarmData(",LS,")");                                                                                                     //DEBUG
    if(swarm_in_focus(_, _, _, _, _)){
        .abolish(swarm_in_focus(_,_,_,_,_));
    }
    +swarm_in_focus(LS, -1,-1, 0, -1);
    //.print("reset swarm: ", LS);                                                                                                            //DEBUG
    for(.member(S,LS)){
        //.print("updateSwarmData(", S, " of ", LS, ")");                                                                                     //DEBUG
        ?pos_agent(SX,SY)[source(S)];
        ?swarm_in_focus(LS, CX,CY, Size, R);
        jia.update_swarm_data(CX, CY, Size, R, SX, SY, New_CX, New_CY, New_Size, New_R);
        .abolish(swarm_in_focus(_,_,_,_,_));
        +swarm_in_focus(LS, New_CX,New_CY, New_Size, New_R);
        .print("Swarm updated: Center (", New_CX, ",", New_CY, "); Size: ", New_Size, "; Radius: ", New_R );                                  //DEBUG
    }.