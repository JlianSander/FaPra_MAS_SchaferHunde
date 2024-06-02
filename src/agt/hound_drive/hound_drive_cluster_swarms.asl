//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 

limit_radius_swarm(5).

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- mapSwarms -------------------------------------------------------

+!mapSwarms 
    <- //.print("mapSwarms");                                                                                                               //DEBUG
    if(swarm(_, _, _, _, _)){
        .abolish(swarm(_, _, _, _, _));  
    }

    //get all known sheep, which are not in the corral, in one big swarm
    .setof(S, pos_agent(_,_)[source(S)] & not is_in_corral(S) & sheep(S) , All_Sheep);
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
                
        if(outsiders(Outsiders)){
            -outsiders(_);
            !mapSetToSwarms(Outsiders);
        }
    } else{
        //.print("Set ", UnMapped_Set, " has only one element.");                                                                           //DEBUG
        !updateSwarmData(UnMapped_Set);
        ?swarm_data_updated(UnMapped_Set, CX,CY, Size, R);
        +swarm(UnMapped_Set, CX,CY, Size, R);
    }.

//------------------------------------------------------- createSwarmWithoutOutsiders ------------------------------------------------------- 

    +!createSwarmWithoutOutsiders(Set_Sheep)
        <- //.print("createSwarmWithoutOutsiders(", Set_Sheep, ")");                                                                        //DEBUG
        ?limit_radius_swarm(Limit_rad);
        if(swarm_data_updated(_, _, _, _, _)){
            .abolish(swarm_data_updated(_,_,_,_,_));
        }
        for(.member(S, Set_Sheep)){
            //.print("createSwarmWithoutOutsiders(", S, " of ", Set_Sheep, ")");                                                            //DEBUG
            ?pos_agent(SX,SY)[source(S)];
            if(swarm_data_updated(_, _, _, _, _)){
                ?swarm_data_updated(LS, CX,CY, Size, R);
                //.print("swarm_data_updated:",LS,CX,CY, Size, R);                                                                          //DEBUG
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
                        .set.add(LS,S);
                        !updateSwarmData(LS);
                    }else{
                        .print("!!!!!!!!!!!!!!!!!!!! ERROR");                                                                               //DEBUG
                        .print("!!!!!!!!!!!!!!!!!!!! LS: ", LS);                                                                            //DEBUG
                        .print("!!!!!!!!!!!!!!!!!!!! S: ", S);                                                                              //DEBUG
                    }
                }
            }else{
                .set.create(New_Swarm);
                .set.add(New_Swarm, S);
                !updateSwarmData(New_Swarm);
            }
        }
        ?swarm_data_updated(New_Swarm, CX,CY, Size, R);
        +swarm(New_Swarm, CX,CY, Size, R);
        .

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////