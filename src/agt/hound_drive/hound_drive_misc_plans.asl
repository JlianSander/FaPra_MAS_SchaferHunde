//------------------------------------------------------- addToSwarm -------------------------------------------------------
+!addToSwarm(Swarm, Sheep)
    <- .set.add(Swarm,Sheep);
    !updateSwarmData(Swarm).

//------------------------------------------------------- guessWhoDrivingWhat -------------------------------------------------------

    swarm_is_in_range_for(H, Ss) :-
        swarm(Ss, CX, CY, R) 
        & pos_agent(HX,HY, H)
        & jia.common.get_distance(HX, HY, CX, CY, D_Ss)
        & offset_to_drive_pos_for_assuming(Offset_To_Drive_Pos)
        & keep_distance_to_sheep(Keep_Distance_to_sheep)
        & D_Ss < R + Keep_Distance_to_sheep + Offset_To_Drive_Pos.


  +!guessWhoDrivingWhat
    <- //.print("guessWhoDrivingWhat");                                                                                                                                     //DEBUG
    if(hound_drives(_,_)){
        .abolish(hound_drives(_, _));
    }

    //get all hounds, which positions are known and all known swarms of sheep
    .setof(H, pos_agent(_,_, H) & hound(H) , All_Hounds);
    //.print("guessWhoDrivingWhat All hounds, which position I know: ", All_Hounds);                                                                                                                  //DEBUG
    .findall(Ss, swarm(Ss, _, _, _), Swarms);
    //.print("guessWhoDrivingWhat All Swarms I know: ", Swarms);                                                                                                                                      //DEBUG
    for(.member(H_in_focus, All_Hounds)){
        //.print("guessWhoDrivingWhat ", H_in_focus, " of ", All_Hounds);
        .findall(Ss_2, .member(Ss_2, Swarms) & swarm_is_in_range_for(H_in_focus, Ss_2), Swarms_in_range);
        //.print("guessWhoDrivingWhat Swarms in range: ", Swarms_in_range);
        !selectSwarmToDrive(Swarms_in_range, H_in_focus);
        if(swarm_chosen_to_drive(_)){
            ?swarm_chosen_to_drive(Swarm_Chosen);
            +hound_drives(H_in_focus, Swarm_Chosen);
            //.print("guessWhoDrivingWhat chosen swarm ", Swarm_Chosen);
        }else{
            //.print("guessWhoDrivingWhat no swarm chosen");
        }

        .abolish(swarm_chosen_to_drive(_));
    }

    //.findall(drives(H,Ss),hound_drives(H, Ss), Drivers);                                                                                                                        //DEBUG
    //.print("Guessed: ", Drivers);                                                                                                                                               //DEBUG
    .

    //------------------------------------------------------- updateSwarmData -------------------------------------------------------

+!updateSwarmData(LS)
    <- //.print("updateSwarmData(",LS,")");                                                                                                 //DEBUG
    .findall(X, pos_agent(X, Y, S) & .member(S,LS), List_of_X);
    .findall(Y, pos_agent(X, Y, S) & .member(S,LS), List_of_Y);
    CX = math.round(math.mean(List_of_X));
    CY = math.round(math.mean(List_of_Y));
    .findall(R, pos_agent(X,Y, S) & .member(S,LS) & jia.common.get_distance(CX,CY,X,Y,R), List_of_R);   
    R = math.round(math.max(List_of_R));
    .abolish(swarm(LS,_,_,_));
    +swarm(LS, CX, CY, R);
    //.print("Updated swarm: (", LS, ", ", CX, ", ", CY, ", ", R, ")");                                                                     //DEBUG
    .