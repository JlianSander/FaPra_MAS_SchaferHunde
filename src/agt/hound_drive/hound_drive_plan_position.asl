//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////

distance_me_to_pos(X,Y, D_Me):- pos(Me_X, Me_Y)  & jia.get_distance(X,Y,Me_X,Me_Y,D_Me).

distance_other_to_pos(X,Y,H, DH):- 
    pos_agent(HX,HY, H) & hound(H) & jia.get_distance(X,Y,HX,HY,DH).

is_closer_to_pos(X,Y,H) :- 
    distance_other_to_pos(X,Y,H, DH) & 
    distance_me_to_pos(X,Y, D_Me) &
    DH < D_Me.
    
is_equal_away_to_pos(X,Y,H) :- 
    distance_other_to_pos(X,Y,H, DH) & 
    distance_me_to_pos(X,Y, D_Me) &
    DH = D_Me.

i_am_lower_than(H):- .my_name(Me) & Me < H.

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- planPositionToDrive ------------------------------------------------------- 

/* +!planPositionToDrive(Swarm) 
    <- .print("planPositionToDrive(", Swarm, ")");                                                                                                      //DEBUG
    ?swarm_data_updated(Swarm, CX,CY, Size, R);
    jia.get_pos_drive_swarm(CX, CY, R, 1, POS_1_X, POS_1_Y);
    jia.get_pos_drive_swarm(CX, CY, R, 2, POS_2_X, POS_2_Y);
    jia.get_pos_drive_swarm(CX, CY, R, 3, POS_3_X, POS_3_Y);
    jia.get_pos_drive_swarm(CX, CY, R, 4, POS_4_X, POS_4_Y);
    jia.get_pos_drive_swarm(CX, CY, R, 5, POS_5_X, POS_5_Y);
    .print("Swarm: (", CX, ",", CY, ") Pos_1: (", POS_1_X, ",", POS_1_Y, ")");
    .print("Swarm: (", CX, ",", CY, ") Pos_2: (", POS_2_X, ",", POS_2_Y, ")");
    .print("Swarm: (", CX, ",", CY, ") Pos_3: (", POS_3_X, ",", POS_3_Y, ")");
    .print("Swarm: (", CX, ",", CY, ") Pos_4: (", POS_4_X, ",", POS_4_Y, ")");
    .print("Swarm: (", CX, ",", CY, ") Pos_5: (", POS_5_X, ",", POS_5_Y, ")");
    -+driving_position(3);
    .print("driving_position(3)"). */


+!planPositionToDrive(Swarm) 
    <- //.print("planPositionToDrive(", Swarm, ")");                                                                                                    //DEBUG
    !guess_who_is_driving_what;
    .findall(H, hound_drives(H, Swarm), Other_Drivers);
    .length(Other_Drivers, Len_Other_Drivers);
    //.print("Other hound driving same swarm: ", Other_Drivers);                                                                                        //DEBUG
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
        !getHoundClosestToPos(Swarm, Other_Drivers, 1);
        ?closest(C);
        if(.my_name(C)){
            -+driving_position(1);
            .print("driving_position(", 1, ")");                                                                                                      //DEBUG
        }else{
            .delete(C, Other_Drivers, New_Other_Drivers);
            !getHoundClosestToPos(Swarm, New_Other_Drivers, 3);
            ?closest(C1);
            if(.my_name(C1)){
                -+driving_position(3);
                .print("driving_position(", 3, ")");                                                                                                  //DEBUG
            }else{
                -+driving_position(5);
                .print("driving_position(", 5, ")");                                                                                                  //DEBUG
            }
        }   
    }
.

+!getHoundClosestToPos(Swarm, Other_Drivers, I)
    <- //.print("getHoundClosestToPos(", Swarm, ", ", Other_Drivers, ", ", I, ")");                                                                       //DEBUG
    !updateSwarmData(Swarm);
    ?swarm_data_updated(Swarm, CX,CY, Size, R);
    jia.get_pos_drive_swarm(CX, CY, R, I, POS_I_X, POS_I_Y);
    /* .print("Position to reach POS_I:(", POS_I_X, ", ", POS_I_Y, ")");                                                                                //DEBUG
    .findall(H_tmp, .member(H_tmp, Other_Drivers) & pos_agent(HX_tmp,HY_tmp, H_tmp), List_tmp);                                                 //DEBUG
    print("List_tmp: ", List_temp);                                                                                                                     //DEBUG */
    .findall(D, .member(H2, Other_Drivers) & pos_agent(HX2,HY2, H2) & jia.get_distance(POS_I_X, POS_I_Y, HX2, HY2, D), List_Distances);
    //.print("Distances to Pos", I, ": ", List_Distances);                                                                                              //DEBUG
    .min(List_Distances, Min_D);
    .findall(H3, .member(H3, Other_Drivers) & pos_agent(HX3,HY3, H3) & jia.get_distance(POS_I_X, POS_I_Y,HX3,HY3,D3) & D3 == Min_D, List_temp);
    .nth(0, List_temp, H);
    /*.print("Other Hound with minimal distance to Pos", I, " is ", H);                                                                                 //DEBUG
    .my_name(Me);                                                                                                                                       //DEBUG
    ?pos(ME_X, ME_Y);                                                                                                                                   //DEBUG
    jia.get_distance(POS_I_X, POS_I_Y, ME_X, ME_Y, D_ME);                                                                                               //DEBUG
    ?pos_agent(HX,HY, H);                                                                                                                       //DEBUG
    jia.get_distance(POS_I_X, POS_I_Y, HX, HY, DH);                                                                                                     //DEBUG
    .print("My distance: ", D_ME, " Other hound ", H, "'s distance: ", DH);                                                                             //DEBUG */
    if(is_closer_to_pos(POS_I_X, POS_I_Y, H)){
        //.print(H, " is closer.");                                                                                                                     //DEBUG
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
    //.print("Hound with minimal distance to Pos", I, " is ", C);                                                                                       //DEBUG
.

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("./hound_drive/hound_drive_choose_swarm.asl")}   