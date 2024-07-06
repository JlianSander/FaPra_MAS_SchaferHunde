//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 



//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- choose_Strategy_select_Swarm -------------------------------------------------------

+! chooseStrategy_clusterSwarms(StratID)
    <- if(StratID == 1){
        !startStrategy_clusterSwarm_1;
    }elif(StratID == 2){
        !startStrategy_clusterSwarm_2;
    }elif(StratID == 3){
        !startStrategy_clusterSwarm_3;
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
        .print("no sheep found");                                                                                                            //DEBUG
        .fail_goal(startStrategy_clusterSwarm_1);
    }
    //.print("All Sheeps not in Corral are: ", All_Sheep);                                                                                    //DEBUG
    !s1_mapSetToSwarms(All_Sheep);
    //.setof(SsDEBUG, swarm(SsDEBUG, _, _, _), SwarmsDEBUG);                                                                                  //DEBUG
    //.print("all found swarms ", SwarmsDEBUG);
    .

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
        ?cluster_swarm_limit_distance_member(Limit_rad);
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
                        !addToSwarm(LS, S);
                    }else{
                        .print("!!!!!!!!!!!!!!!!!!!! ERROR");                                                                               //DEBUG
                        .print("!!!!!!!!!!!!!!!!!!!! LS: ", LS);                                                                            //DEBUG
                        .print("!!!!!!!!!!!!!!!!!!!! S: ", S);                                                                              //DEBUG
                    }
                }
            }else{
                .set.create(New_Swarm);
                !addToSwarm(New_Swarm, S);
                +swarm_temp(New_Swarm);
            }
        }.

//------------------------------------------------------- startStrategy_clusterSwarm_2 (single linkage) -------------------------------------------------------
+!startStrategy_clusterSwarm_2
    <- //.print("startStrategy_clusterSwarm_2");
    //get all known sheep, which are not in the corral, in one big set
    .setof(S, pos_agent(_,_, S) & not is_in_corral(S) & sheep(S) , All_Sheep);
    if(.length(All_Sheep, 0)){
        .print("no sheep found");                                                                                                               //DEBUG
        .fail_goal(startStrategy_clusterSwarm_2);
    }

    for(.member(S, All_Sheep)){
        //make each sheep to one swarm
        //.print("startStrategy_clusterSwarm_3 --- no swarm close enough");                                                                   //DEBUG
        .set.create(New_Swarm);
        !addToSwarm(New_Swarm, S);
    } 
    !s2_mergeSwarmsSingleLinkage;
    //.setof(SsDEBUG2, swarm(SsDEBUG2, _, _, _), SwarmsDEBUG2);                                                                                  //DEBUG
    //.print("all found swarms ", SwarmsDEBUG2);                                                                                               //DEBUG
    .



+!s2_mergeSwarmsSingleLinkage
    <- //.print("s2_mergeSwarmsSingleLinkage");
    // do as long as there are more than 2 swarms close to each other
    while(exists_close_swarms_single_linkage){
        //always process only first element of swarms, which are close to another swarm
        .setof(TmpSs3, swarm(TmpSs3, _, _, _) & swarm(TmpSs4, _, _, _) & TmpSs3 \== TmpSs4 & swarms_are_close_to_eachother_single_linkage(TmpSs3, TmpSs4), ListSwarms);
        .nth(0, ListSwarms, Ss1);
        //merge with closest swarm
        .setof(TmpD, swarm(TmpSs2, _, _, _) & distance_between_swarms_closest_members(Ss1, TmpSs2, TmpD), Distances_Swarms);
        .min(Distances_Swarms, Min_D);
        .findall(TmpSs5, swarm(TmpSs5, _, _, _) & distance_between_swarms_closest_members(Ss1, TmpSs5, TmpD1) & TmpD1 == Min_D, Closest_Swarms);
        .nth(0, Closest_Swarms, Ss2);
        .set.union(Ss1, Ss2, Ss1);
        -swarm(Ss2, _, _, _);
        !updateSwarmData(Ss1);
    }.

//------------------------------------------------------- startStrategy_clusterSwarm_3 (complete linkage) -------------------------------------------------------
+!startStrategy_clusterSwarm_3
    <- //.print("startStrategy_clusterSwarm_3");
    //get all known sheep, which are not in the corral, in one big set
    .setof(S, pos_agent(_,_, S) & not is_in_corral(S) & sheep(S) , All_Sheep);
    if(.length(All_Sheep, 0)){
        .print("no sheep found");                                                                                                               //DEBUG
        .fail_goal(startStrategy_clusterSwarm_2);
    }

    for(.member(S, All_Sheep)){
        //make each sheep to one swarm
        //.print("startStrategy_clusterSwarm_3 --- no swarm close enough");                                                                   //DEBUG
        .set.create(New_Swarm);
        !addToSwarm(New_Swarm, S);
    }
    !s3_mergeSwarmsCompleteLinkage;
    //.setof(SsDEBUG, swarm(SsDEBUG, _, _, _), SwarmsDEBUG);                                                                                  //DEBUG
    //.print("all found swarms ", SwarmsDEBUG);                                                                                               //DEBUG
    .

+!s3_mergeSwarmsCompleteLinkage
    <- //.print("s3_mergeSwarmsCompleteLinkage");
    // do as long as there are more than 2 swarms close to each other
    while(exists_close_swarms_complete_linkage){
        //always process only first element of swarms, which are close to another swarm
        .setof(TmpSs3, swarm(TmpSs3, _, _, _) & swarm(TmpSs4, _, _, _) & TmpSs3 \== TmpSs4 & swarms_are_close_to_eachother_complete_linkage(TmpSs3, TmpSs4), ListSwarms);
        .nth(0, ListSwarms, Ss1);
        //merge with closest swarm
        .setof(TmpD, swarm(TmpSs2, _, _, _) & distance_between_swarms_farest_members(Ss1, TmpSs2, TmpD), Distances_Swarms);
        .min(Distances_Swarms, Min_D);
        .findall(TmpSs5, swarm(TmpSs5, _, _, _) & distance_between_swarms_farest_members(Ss1, TmpSs5, TmpD1) & TmpD1 == Min_D, Closest_Swarms);
        .nth(0, Closest_Swarms, Ss2);
        .set.union(Ss1, Ss2, Ss1);
        -swarm(Ss2, _, _, _);
        !updateSwarmData(Ss1);
    }.