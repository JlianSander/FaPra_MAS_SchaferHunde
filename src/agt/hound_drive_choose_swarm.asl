
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////  

other_hound_is_closer_to_swarm(Swarm) :- .member(S, Swarm) & other_hound_is_closer_to_sheep(S).

is_closer_to_swarm(H, Ss):- swarm(Ss, CX, CY, Size, R) & 
    pos_agent(HX,HY)[source(H)] & hound(H) & jia.get_distance(CX,CY,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(CX,CY,ME_X,ME_Y,D_ME) &
    DH < D_ME.

limit_distance_assumption_hound_driving(2).

limit_number_agents_driving_swarm(3).

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- chooseSwarmToDrive -------------------------------------------------------
+!chooseSwarmToDrive(Swarms)
    <- .print("chooseSwarmToDrive(", Swarms, ")");                                                                                                                          //DEBUG
    if(swarm_chosen_to_drive(_)){
        .abolish(swarm_chosen_to_drive(_));
    }

    for(.member(Swarm_to_Evaluate, Swarms)){
        .findall(H, hound_drives(H, Swarm_to_Evaluate) & is_closer_to_swarm(H, Swarm_to_Evaluate), Drivers);
        .length(Drivers, Len_Drivers);
        ?limit_number_agents_driving_swarm(Limit_Num_Agts_Driving);
        //only choose swarm to drive if agent thinks agents driving this swarm are not too many
        if(Len_Drivers < Limit_Num_Agts_Driving){
            if(not swarm_chosen_to_drive(_)){
                +swarm_chosen_to_drive(Swarm_to_Evaluate);
            }else{
                ?swarm_chosen_to_drive(Swarm_Chosen);
                //.print("Swarm_to_Evaluate: ", Swarm_to_Evaluate);                                                                                                               //DEBUG
                //.print("Swarm_Chosen: ", Swarm_Chosen);                                                                                                                         //DEBUG
                //choose new swarm if it contains more sheep
                if(.length(Swarm_to_Evaluate, Len_Eval) & .length(Swarm_Chosen, Len_Chosen) & Len_Eval > Len_Chosen){
                    -+swarm_chosen_to_drive(Swarm_to_Evaluate);
                }
            }
        }
    }.

  +!guess_who_is_driving_what
    <-
    if(hound_drives(_,_)){
        .abolish(hound_drives(_, _));
    }

    //get all hounds and swarms
    .setof(H, pos_agent(_,_)[source(H)] & hound(H) , All_Hounds);
    .findall(Ss, swarm(Ss, _, _, _, _), Swarms);
    ?limit_distance_assumption_hound_driving(Limit_Distance_Driving);
    for(.member(H_in_focus, All_Hounds)){
        ?pos_agent(HX,HY)[source(H_in_focus)];
        for(.member(Ss_2, Swarms)){
            ?swarm(Ss_2, CX_2, CY_2, Size, R);
            jia.get_distance(HX,HY,CX_2,CY_2,D_Ss_2);

            //check if hound is within limit to drive, otherwise suspect that hound is not driving the swarm
            if(D_Ss_2 < Limit_Distance_Driving){
                if(not hound_drives(H_in_focus,_)){
                    +hound_drives(H_in_focus, Ss_2);
                }else{
                    // suspect hound to drive the swarm he's closer to
                    ?hound_drives(H_in_focus, Ss_3);
                    ?swarm(Ss_3, CX_3, CY_3, Size, R);
                    jia.get_distance(HX,HY,CX_3,CY_3,D_Ss_3);
                    if(D_Ss_3 < D_Ss_2){
                        -+hound_drives(H_in_focus, Ss_3);
                    }
                }
            }
        }
    }
    .
//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////