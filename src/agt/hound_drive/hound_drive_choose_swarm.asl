
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////  

other_hound_is_closer_to_swarm(Swarm) :- .member(S, Swarm) & other_hound_is_closer_to_sheep(S).

is_closer_to_swarm(H, Ss):- swarm(Ss, CX, CY, Size, R) & 
    pos_agent(HX,HY)[source(H)] & hound(H) & jia.get_distance(CX,CY,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(CX,CY,ME_X,ME_Y,D_ME) &
    DH < D_ME.

i_am_close_enough_to_swarm(Ss):- limit_distance_assumption_hound_driving(Limit_Distance_Driving) &
    swarm(Ss, CX, CY, Size, R) &
    pos(ME_X, ME_Y)  & jia.get_distance(CX,CY,ME_X,ME_Y,D_ME) &
    D_ME < Limit_Distance_Driving.


limit_distance_assumption_hound_driving(7).

limit_number_agents_driving_swarm(3).

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- chooseSwarmToDrive -------------------------------------------------------
+!chooseSwarmToDrive(Swarms)
    <- //.print("chooseSwarmToDrive(", Swarms, ")");                                                                                                                              //DEBUG
    if(swarm_chosen_to_drive(_)){
        .abolish(swarm_chosen_to_drive(_));
    }
    !guess_who_is_driving_what;

    for(.member(Swarm_to_Evaluate, Swarms)){
        //.print(Swarm_to_Evaluate, " of ", Swarms);                                                                                                                              //DEBUG
        //?swarm(Swarm_to_Evaluate, CX, CY, Size, R);                                                                                                                             //DEBUG
        //.print("Swarm: ", Swarm_to_Evaluate, " Center: (", CX, ",", XY, ")");                                                                                                   //DEBUG
        .setof(H, hound_drives(H, Swarm_to_Evaluate) & is_closer_to_swarm(H, Swarm_to_Evaluate), Drivers);
        .length(Drivers, Len_Drivers);
        //.setof(H, hound_drives(H, Swarm_to_Evaluate) , Drivers_Debug);                                                                                                          //DEBUG
        //.print("Drivers closer to swarm: ", Drivers, " Length: ", Len_Drivers, " All Drivers: ", Drivers_Debug);                                                                //DEBUG                                                                                                                                                     //DEBUG
        ?limit_number_agents_driving_swarm(Limit_Num_Agts_Driving);
        //only choose swarm to drive if agent thinks agents driving this swarm are not enough
        if(Len_Drivers < Limit_Num_Agts_Driving & i_am_close_enough_to_swarm(Swarm_to_Evaluate)){
            if(not swarm_chosen_to_drive(_)){
                +swarm_chosen_to_drive(Swarm_to_Evaluate);
            }else{
                ?swarm_chosen_to_drive(Swarm_Chosen);
                //.print("Swarm_to_Evaluate: ", Swarm_to_Evaluate);                                                                                                             //DEBUG
                //.print("Swarm_Chosen: ", Swarm_Chosen);                                                                                                                       //DEBUG
                //choose new swarm if it contains more sheep
                if(.length(Swarm_to_Evaluate, Len_Eval) & .length(Swarm_Chosen, Len_Chosen) & Len_Eval > Len_Chosen){
                    -+swarm_chosen_to_drive(Swarm_to_Evaluate);
                    //.print("Driving new swarm, since this one has more members.");                                                                                            //DEBUG
                }else{
                    //.print("Staying with old chosen swarm, since this one has more members.");                                                                                //DEBUG
                }
            }
        } else{
            if( i_am_close_enough_to_swarm(Swarm_to_Evaluate)){
                //.print("Not driving swarm ", Swarm_to_Evaluate, " since it has enough drivers: ", Len_Drivers);                                                                 //DEBUG
            }else{
                //.print("I'm not close enough to drive the swarm: ", Swarm_to_Evaluate);                                                                                         //DEBUG
            }            
        }
    }
    
    if(not swarm_chosen_to_drive(_)){
        .fail_goal(chooseSwarmToDrive(Swarms));
        //.print("Not chosen any swarm.");                                                                                                                                        //DEBUG
    }else{
        ?swarm_chosen_to_drive(Ss);
        //.print("Swarm chosen: ", Ss);                                                                                                                                           //DEBUG
    }.

//------------------------------------------------------- guess_who_is_driving_what -------------------------------------------------------

  +!guess_who_is_driving_what
    <- //.print("guess_who_is_driving_what");                                                                                                                                     //DEBUG
    if(hound_drives(_,_)){
        .abolish(hound_drives(_, _));
    }

    //get all hounds, of which their positions are known and all known swarms of sheep
    .setof(H, pos_agent(_,_)[source(H)] & hound(H) , All_Hounds);
    //.print("All hounds, which position I know: ", All_Hounds);                                                                                                                  //DEBUG
    .findall(Ss, swarm(Ss, _, _, _, _), Swarms);
    //.print("All Swarms I know: ", Swarms);                                                                                                                                      //DEBUG
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
                    if(D_Ss_2 < D_Ss_3){
                        -+hound_drives(H_in_focus, Ss_2);
                    }
                }
            }
        }
    }

    //.findall(drives(H,Ss),hound_drives(H, Ss), Drivers);                                                                                                                        //DEBUG
    //.print("Guessed: ", Drivers);                                                                                                                                               //DEBUG
    .
//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////