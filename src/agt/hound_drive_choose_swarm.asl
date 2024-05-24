//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////  

other_hound_is_closer(Swarm) :- .member(S, Swarm) & other_hound_is_closer(S).


//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- chooseSwarmToDrive -------------------------------------------------------
+!chooseSwarmToDrive(Swarms)
    <- .print("chooseSwarmToDrive(", Swarms, ")");                                                                                                                          //DEBUG
    if(swarm_chosen_to_drive(_)){
        .abolish(swarm_chosen_to_drive(_));
    }

    for(.member(Swarm_to_Evaluate, Swarms)){
        if(not swarm_chosen_to_drive(_)){
            +swarm_chosen_to_drive(Swarm_to_Evaluate);
        }else{
            ?swarm_chosen_to_drive(Swarm_Chosen);
            //.print("Swarm_to_Evaluate: ", Swarm_to_Evaluate);                                                                                                               //DEBUG
            //.print("Swarm_Chosen: ", Swarm_Chosen);                                                                                                                         //DEBUG
            if(.length(Swarm_to_Evaluate, Len_Eval) & .length(Swarm_Chosen, Len_Chosen) & Len_Eval > Len_Chosen & not other_hound_is_closer(Swarm_to_Evaluate)){
                -+swarm_chosen_to_drive(Swarm_to_Evaluate);
            }
        }
    }.

/* +!guess_who_is_driving_what
    <-
    .setof(H, pos_agent(_,_)[source(H)] & hound(H) , All_Hounds);
    .findall(Ss, known_swarms(Ss),Swarms);
    for(.member(H_in_focus, All_Hounds)){

    }
    . */
//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////