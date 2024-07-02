//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 



//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- choose_Strategy_select_Swarm -------------------------------------------------------

+! chooseStrategy_clusterSwarms(StratID)
    <- if(StratID == 1){
        !startStrategy_clusterSwarm_1;
    }elif(StratID == 2){
        !startStrategy_clusterSwarm_2;
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
        ?cluster_swarm_limit_radius(Limit_rad);
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

//------------------------------------------------------- startStrategy_clusterSwarm_2 -------------------------------------------------------
+!startStrategy_clusterSwarm_2
    <- //.print("startStrategy_clusterSwarm_2");
    //get all known sheep, which are not in the corral, in one big set
    .setof(S, pos_agent(_,_, S) & not is_in_corral(S) & sheep(S) , All_Sheep);
    if(.length(All_Sheep, 0)){
        .print("no sheep found");                                                                                                               //DEBUG
        .fail_goal(startStrategy_clusterSwarm_2);
    }

    for(.member(S, All_Sheep)){
        //.print("startStrategy_clusterSwarm_2 --- ", S, " in ", All_Sheep);                                                                      //DEBUG
        //get all swarms which have a member close enough to sheep
        ?cluster_swarm_limit_closest_member(Limit_distance);
        //.setof(SsDEBUG, swarm(SsDEBUG, _, _, _), SwarmsDEBUG);                                                                                  //DEBUG
        //.print("all swarms known ", SwarmsDEBUG);                                                                                               //DEBUG
        .setof(Ss, swarm(Ss, _, _, _) & .member(S1, Ss) & distance_between_agents(S, S1, D) & D < Limit_distance, Swarms);
        //.print("startStrategy_clusterSwarm_2 --- swarms close enough ", Swarms);                                                                //DEBUG
        if(.length(Swarms, Len_Swarms) & Len_Swarms == 0){
            //no swarm is close enough
            //.print("startStrategy_clusterSwarm_2 --- no swarm close enough");                                                                   //DEBUG
            .set.create(New_Swarm);
            !addToSwarm(New_Swarm, S);
        }else{
            //map sheep to the swarm to whose closest member the sheep is closest
            .setof(D_Member, .member(Ss1, Swarms) & .member(S2, Ss1) & distance_between_agents(S, S2, D_Member), Distances_Members);
            //.print("startStrategy_clusterSwarm_2 --- distances of members" , Distances_Members);                                                  //DEBUG
            .min(Distances_Members, Min_Distance_Member);
            //.print("startStrategy_clusterSwarm_2 --- Minimum Distance ", Min_Distance_Member);                                                    //DEBUG
            .setof(Ss2, .member(Ss2, Swarms) & .member(S3, Ss2) & distance_between_agents(S, S3, Min_Distance_Member) , Pos_Swarms);
            //.print("startStrategy_clusterSwarm_2 --- Pos_Swarms ", Pos_Swarms);                                                                   //DEBUG
            .length(Pos_Swarms, Len_PosSwarms);
            .nth(0, Pos_Swarms, Swarm_to_Map);
            //.print("startStrategy_clusterSwarm_2 ---  Swarm_to_Map ", Swarm_to_Map);                                                              //DEBUG
            !addToSwarm(Swarm_to_Map, S);
        }
    }

    !s2_mergeCloseSwarms;

    //.setof(SsDEBUG, swarm(SsDEBUG, _, _, _), SwarmsDEBUG);                                                                                  //DEBUG
    //.print("all found swarms ", SwarmsDEBUG);                                                                                               //DEBUG
    .



+!s2_mergeCloseSwarms
    <- //.print("s2_mergeCloseSwarms");
    // do as long as there are more than 2 swarms close to each other
    while(exists_close_swarms){
        //.print("s2_mergeCloseSwarms --- has close swarms");
        //always process only first element of swarms, which are close to another swarm
        .setof(TmpSs3, swarm(TmpSs3, _, _, _) & swarm(TmpSs4, _, _, _) & TmpSs3 \== TmpSs4 & swarms_are_close_to_eachother(TmpSs3, TmpSs4), ListSwarms);
        .nth(0, ListSwarms, Ss1);
        //find all elements in the list which are close to the swarm in focus
        .setof(TmpSs2, .member(TmpSs2, ListSwarms) & swarms_are_close_to_eachother(Ss1, TmpSs2), ListCloseSwarms);
        if(.length(ListCloseSwarms, TmpListCloseSwarms_Len) & TmpListCloseSwarms_Len == 0){
            .print("ERROR No other swarm is close to ", Ss1,  " in ", ListSwarms);
        }else{
            //.print("Create union of ", Ss1, " and ", ListCloseSwarms);                                                                                             //DEBUG
            for(.member(Ss2, ListCloseSwarms)){
                .set.union(Ss1, Ss2, Ss1);
                -swarm(Ss2, _, _, _);
            }
            !updateSwarmData(Ss1);
            //do not remove Ss1 from ListSwarms, since the swarm can be unified with other swarms who have become close, since Ss1 got new elements
        }
    }.