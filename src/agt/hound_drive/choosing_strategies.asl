//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- choosing_Strategy_1 -------------------------------------------------------

+! choosing_Strategy_1(Swarm_to_Evaluate)
    <- ?swarm_chosen_to_drive(Swarm_Chosen);
    //.print("Swarm_to_Evaluate: ", Swarm_to_Evaluate);                                                                                                             //DEBUG
    //.print("Swarm_Chosen: ", Swarm_Chosen);                                                                                                                       //DEBUG
    //choose new swarm if it contains more sheep
    if(.length(Swarm_to_Evaluate, Len_Eval) & .length(Swarm_Chosen, Len_Chosen) & Len_Eval > Len_Chosen){
        //TODO Nähe zum Schwarm in Entscheidung einfließen lassen
        -+swarm_chosen_to_drive(Swarm_to_Evaluate);
        //.print("Driving new swarm, since this one has more members.");                                                                                            //DEBUG
    }else{
        //.print("Staying with old chosen swarm, since this one has more members.");                                                                                //DEBUG
    }.