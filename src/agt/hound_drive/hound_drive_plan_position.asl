//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////



//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- planPositionToDrive ------------------------------------------------------- 

+!planPositionToDrive(Swarm) 
    <- .print("planPositionToDrive(", Swarm, ")");                                                                                                    //DEBUG
    !guessWhoDrivingWhat;
    .findall(H, hound_drives(H, Swarm), Other_Drivers);
    .length(Other_Drivers, Len_Other_Drivers);
    .print("Other hound driving same swarm: ", Other_Drivers);                                                                                        //DEBUG
    if(Len_Other_Drivers < 1){
        //I'm the only hound driving this swarm
        -+driving_position(3);
        .print("driving_position(3)");                                                                                                                //DEBUG
    }elif(Len_Other_Drivers < 2){
        //there is another hound driving the swarm
        !getHoundClosestToPos(Swarm, Other_Drivers, 2);
        ?closest(C);
        if(.my_name(C)){
            -+driving_position(2);
            .print("driving_position(", 2, ")");                                                                                                      //DEBUG
        }else{
            -+driving_position(4);
            .print("driving_position(", 4, ")");                                                                                                      //DEBUG
        }
    }else{
        // there are two other hounds driving the same swarm
        !getHoundClosestToPos(Swarm, Other_Drivers, 3);
        ?closest(C);
        if(.my_name(C)){
            -+driving_position(3);
            .print("driving_position(", 3, ")");                                                                                                      //DEBUG
        }else{
            .delete(C, Other_Drivers, New_Other_Drivers);
            !getHoundClosestToPos(Swarm, New_Other_Drivers, 1);
            ?closest(C1);
            if(.my_name(C1)){
                -+driving_position(1);
                .print("driving_position(", 1, ")");                                                                                                  //DEBUG
            }else{
                -+driving_position(5);
                .print("driving_position(", 5, ")");                                                                                                  //DEBUG
            }
        }   
    }
.

+!getHoundClosestToPos(Swarm, Other_Drivers, Position_Idx)
    <- .print("getHoundClosestToPos(", Swarm, ", ", Other_Drivers, ", ", Position_Idx, ")");                                                                       //DEBUG
    !updateSwarmData(Swarm);
    ?swarm(Swarm, CX, CY, R);
    jia.hounds.get_pos_drive_swarm(CX, CY, R, Position_Idx, POS_I_X, POS_I_Y);
    !getClosestHoundToPosCritical(Other_Drivers, POS_I_X, POS_I_Y, H);
    .print("Other Hound with minimal distance to Pos", Position_Idx, " is ", H);                                                                                 //DEBUG
    .my_name(Me);                                                                                                                                       //DEBUG
    ?pos(ME_X, ME_Y);                                                                                                                                   //DEBUG
    jia.common.get_distance(POS_I_X, POS_I_Y, ME_X, ME_Y, D_ME);                                                                                               //DEBUG
    ?pos_agent(HX,HY, H);                                                                                                                       //DEBUG
    jia.common.get_distance(POS_I_X, POS_I_Y, HX, HY, DH);                                                                                                     //DEBUG
    .print("My distance: ", D_ME, " Other hound ", H, "'s distance: ", DH);                                                                             //DEBUG 
    if(is_closer_to_pos(POS_I_X, POS_I_Y, H)){
        .print(H, " is closer.");                                                                                                                     //DEBUG
        -+closest(H);
    }else{
        //.print("I'm closer.");                                                                                                                        //DEBUG
        if(is_equal_away_to_pos(POS_I_X, POS_I_Y, H)){
            if(i_am_lower_than(H)){
                -+closest(Me);
            }
            else{
                -+closest(H);
            }
        }else{
            -+closest(Me);
        }
    }
    ?closest(C);
    .print("Hound with minimal distance to Pos", Position_Idx, " is ", C);                                                                                       //DEBUG
.

@getClosest[atomic]
+!getClosestHoundToPosCritical(Other_Drivers, POS_I_X, POS_I_Y, H)
    <- .print("Position to reach Pos:(", POS_I_X, ", ", POS_I_Y, ")");                                                                                          //DEBUG
    .findall(H_tmp, .member(H_tmp, Other_Drivers) & pos_agent(HX_tmp,HY_tmp, H_tmp), List_tmp);                                                                   //DEBUG
    .print("List_tmp: ", List_tmp);                                                                                                                               //DEBUG
    .findall(D, .member(H2, Other_Drivers) & pos_agent(HX2, HY2, H2) & jia.common.get_distance(POS_I_X, POS_I_Y, HX2, HY2, D), List_Distances);
    .print("Distances to Pos: ", List_Distances);                                                                                               //DEBUG
    .min(List_Distances, Min_D);
    .print("Min_D: ", Min_D);
    .findall(H3, .member(H3, Other_Drivers) & pos_agent(HX3, HY3, H3) & jia.common.get_distance(POS_I_X, POS_I_Y,HX3,HY3,D3) & D3 == Min_D, List_Closest);    //critical since position of the agents must not change between the lines
    .min(List_Closest, H).

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////
 