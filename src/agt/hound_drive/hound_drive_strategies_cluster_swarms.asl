//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 



//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- choose_Strategy_select_Swarm -------------------------------------------------------

+! chooseStrategy_clusterSwarms(StratID)
    <- if(StratID == 1){
        !startStrategy_clusterSwarm_1;
    }else{
        .print("ERROR no such strategy known");
        false;
    }.

//------------------------------------------------------- startStrategy_clusterSwarm_1 -------------------------------------------------------
+!startStrategy_clusterSwarm_1 
    <-
    //get all known sheep, which are not in the corral, in one big set
    .setof(S, pos_agent(_,_, S) & not is_in_corral(S) & sheep(S) , All_Sheep);
    if(.length(All_Sheep, 0)){
        //no swarm found
        .print("no sheep found");                                                                                                            //DEBUG
        .fail_goal(startStrategy_clusterSwarm_1);
    }
    //.print("All Sheeps not in Corral are: ", All_Sheep);                                                                                    //DEBUG
    !s1_mapSetToSwarms(All_Sheep).

//------------------------------------------------------- s1_mapSetToSwarms ------------------------------------------------------- 

+!s1_mapSetToSwarms(UnMapped_Set) 
    <- //.print("s1_mapSetToSwarms(", UnMapped_Set, ")");                                                                                      //DEBUG
    //only calculate swarm for more than one sheep in the set
    if(.length(UnMapped_Set) > 1){
        !s1_createSwarmWithoutOutsiders(UnMapped_Set);
                
        if(outsiders(Outsiders)){
            -outsiders(_);
            !s1_mapSetToSwarms(Outsiders);
        }
    } else{
        //.print("Set ", UnMapped_Set, " has only one element.");                                                                           //DEBUG
        !updateSwarmData(UnMapped_Set);
    }.

//------------------------------------------------------- s1_createSwarmWithoutOutsiders ------------------------------------------------------- 

    +!s1_createSwarmWithoutOutsiders(Set_Sheep)
        <- //.print("s1_createSwarmWithoutOutsiders(", Set_Sheep, ")");                                                                        //DEBUG
        ?limit_radius_swarm(Limit_rad);
        if(swarm_temp(_)){
            .abolish(swarm_temp(_));
        }
        for(.member(S, Set_Sheep)){
            //.print("s1_createSwarmWithoutOutsiders(", S, " of ", Set_Sheep, ")");                                                            //DEBUG
            ?pos_agent(SX,SY, S);
            if(swarm_temp(_)){
                ?swarm_temp(LS);
                ?swarm(LS, CX, CY, R);
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
                +swarm_temp(New_Swarm);
            }
        }.

