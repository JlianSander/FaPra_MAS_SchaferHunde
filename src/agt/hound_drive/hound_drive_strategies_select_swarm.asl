//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- choose_Strategy_select_Swarm -------------------------------------------------------

+! chooseStrategy_selectSwarm(Swarm, Hme, StratID)
    <- if(StratID == 1){
        !startStrategy_selectSwarm_1(Swarm, Hme);
    }elif(StratID == 2){
        !startStrategy_selectSwarm_2(Swarm, Hme);
    }else{
        .print("ERROR no such strategy known");
        false;
    }.

//------------------------------------------------------- startStrategy_selectSwarm_1 -------------------------------------------------------

+! startStrategy_selectSwarm_1(Swarm_to_Evaluate, Hme)
    <- ?swarm_chosen_to_drive(Swarm_Chosen);
    //.print("startStrategy_selectSwarm_1 (", Swarm_to_Evaluate, ")");                                                                                              //DEBUG
    //choose new swarm if it contains more sheep
    if(.length(Swarm_to_Evaluate, Len_Eval) & .length(Swarm_Chosen, Len_Chosen) & Len_Eval > Len_Chosen){
        //TODO Nähe zum Schwarm in Entscheidung einfließen lassen
        -+swarm_chosen_to_drive(Swarm_to_Evaluate);
        //.print("Driving new swarm, since this one has more members.");                                                                                            //DEBUG
    }else{
        //.print("Staying with old chosen swarm, since this one has more members.");                                                                                //DEBUG
    }.

//------------------------------------------------------- startStrategy_selectSwarm_2 -------------------------------------------------------

+! startStrategy_selectSwarm_2(Swarm_to_Evaluate, H)
    <- 
    //.print("startStrategy_selectSwarm_2 (", Swarm_to_Evaluate, ")");                                                                                      //DEBUG
    ?swarm_chosen_to_drive(Swarm_Chosen);
    //.print("startStrategy_selectSwarm_2 --- swarm chosen: ", Swarm_Chosen);                                                                               //DEBUG

    .length(Swarm_to_Evaluate, Len_Eval);
    .length(Swarm_Chosen, Len_Chosen);
    if(Len_Eval > Len_Chosen){
        Decision_Size = 1;
        //.print(Swarm_to_Evaluate , " has bigger size.");                                                                                                  //DEBUG
    }elif(Len_Eval < Len_Chosen){
        Decision_Size = -1;
        //.print(Swarm_Chosen , " has bigger size.");                                                                                                       //DEBUG
    }else{
        Decision_Size = 0;
        //.print(Swarm_Chosen , " and ", Swarm_to_Evaluate, " are of equal size.");                                                                         //DEBUG
    }
    
    .my_name(Me);
    if(H == Me){
        ?distance_me_to_swarm(Swarm_to_Evaluate, D_eval);
        ?distance_me_to_swarm(Swarm_Chosen, D_cho);
    }else{
        ?distance_other_hound_to_swarm(Swarm_to_Evaluate, H, D_eval);
        ?distance_other_hound_to_swarm(Swarm_Chosen, H, D_cho);
    }
    
    if(D_eval < D_cho){
        Decision_Proximity = 1;
        //.print(Swarm_to_Evaluate , " is closer.");                                                                                                        //DEBUG
    }elif (D_eval > D_cho){
        Decision_Proximity = -1;
        //.print(Swarm_Chosen , " is closer.");                                                                                                             //DEBUG
    }else{
        Decision_Proximity = 0;
        //.print(Swarm_Chosen , " and ", Swarm_to_Evaluate, " are equally far away.");                                                                      //DEBUG
    }

    ?select_swarm_weight_proximity(Weight_prox);
    ?select_swarm_weight_size(Weight_size);
    if(Decision_Size * Weight_size + Decision_Proximity * Weight_prox > 0){
        //.print("Decision > 0 : choose swarm ", Swarm_to_Evaluate);                                                                                        //DEBUG
        -+swarm_chosen_to_drive(Swarm_to_Evaluate);
    }.