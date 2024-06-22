
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////  

is_closer_to_swarm(H, Ss):- swarm(Ss, CX, CY, R) & 
    pos_agent(HX,HY, H) & hound(H) & jia.get_distance(CX,CY,HX,HY,DH) & 
    my_distance_to_swarm(Ss, D_ME) &
    DH < D_ME.

my_distance_to_swarm(Ss, D):- swarm(Ss, CX, CY, R) & pos(ME_X, ME_Y)  & jia.get_distance(CX,CY,ME_X,ME_Y,D).

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- selectSwarmToDrive -------------------------------------------------------
+!selectSwarmToDrive(Swarms)
    <- //.print("selectSwarmToDrive(", Swarms, ")");                                                                                                                              //DEBUG
    if(swarm_chosen_to_drive(_)){
        .abolish(swarm_chosen_to_drive(_));
    }
    !guess_who_is_driving_what;

    for(.member(Swarm_to_Evaluate, Swarms)){
        //.print(Swarm_to_Evaluate, " of ", Swarms);                                                                                                                              //DEBUG
        //?swarm(Swarm_to_Evaluate, CX, CY, R);                                                                                                                             //DEBUG
        //.print("Swarm: ", Swarm_to_Evaluate, " Center: (", CX, ",", XY, ")");                                                                                                   //DEBUG
        .setof(H, hound_drives(H, Swarm_to_Evaluate) & is_closer_to_swarm(H, Swarm_to_Evaluate), Drivers);
        .length(Drivers, Len_Drivers);
        //.setof(H, hound_drives(H, Swarm_to_Evaluate) , Drivers_Debug);                                                                                                          //DEBUG
        //.print("Drivers closer to swarm: ", Drivers, " Length: ", Len_Drivers, " All Drivers: ", Drivers_Debug);                                                                //DEBUG                                                                                                                                                     //DEBUG
        ?limit_number_agents_driving_swarm(Limit_Num_Agts_Driving);
        //only choose swarm to drive if agent thinks agents driving this swarm are not enough
        if(Len_Drivers < Limit_Num_Agts_Driving){
            if(not swarm_chosen_to_drive(_)){
                +swarm_chosen_to_drive(Swarm_to_Evaluate);
            }else{
                ?strategy_select_swarm(StratID);
                !chooseStrategy_selectSwarm(Swarm_to_Evaluate, StratID);
            }
        } else{
            /* if( i_am_close_enough_to_swarm(Swarm_to_Evaluate)){
                //.print("Not driving swarm ", Swarm_to_Evaluate, " since it has enough drivers: ", Len_Drivers);                                                                 //DEBUG
            }else{
                //.print("I'm not close enough to drive the swarm: ", Swarm_to_Evaluate);                                                                                         //DEBUG
            }  */           
        }
    }
    
    if(not swarm_chosen_to_drive(_)){
        .fail_goal(selectSwarmToDrive(Swarms));
        //.print("Not chosen any swarm.");                                                                                                                                        //DEBUG
    }else{
        //?swarm_chosen_to_drive(Ss);                                                                                                                                             //DEBUG
        //.print("Swarm chosen: ", Ss);                                                                                                                                           //DEBUG
    }.


//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////
{ include("./hound_drive/hound_drive_strategies_select_swarm.asl")}