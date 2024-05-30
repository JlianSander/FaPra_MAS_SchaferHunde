//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////

driving_position(3).

is_closer_to_pos(X,Y,H) :- 
    pos_agent(HX,HY)[source(H)] & hound(H) & jia.get_distance(X,Y,HX,HY,DH) & 
    pos(ME_X, ME_Y)  & jia.get_distance(X,Y,ME_X,ME_Y,D_ME) &
    DH < D_ME.

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

+!planPositionToDrive(Swarm) 
    <- .print("planPositionToDrive(", Swarm, ")");                                                                                          //DEBUG
    !guess_who_is_driving_what;
    .findall(H, hound_drives(H, Swarm), Drivers);
    .length(Drivers, Len_Drivers);
    .print("Other hound driving same swarm: ", Drivers);                                                                                    //DEBUG
    if(Len_Drivers < 1){
        //I'm the only hound driving this swarm
        -+driving_position(3);
        .print("driving_position(3)");                                                                                                      //DEBUG
    }elif(Len_Drivers < 2){
        //there is another hound driving the swarm
        !updateSwarmData(Swarm);
        ?swarm_data_updated(Swarm, CX,CY, Size, R);
        jia.get_pos_drive_swarm(CX, CY, R, 2, POS_2_X, POS_2_Y);
        .nth(0, Drivers, H2);
        ?pos(ME_X, ME_Y);                                                                                                                   //DEBUG
        ?pos_agent(H2X,H2Y)[source(H2)];                                                                                                    //DEBUG
        .print("Pos_ME:", ME_X, "|", ME_Y, " Pos_", H2, ":", H2X, "|", H2Y, " POS_2:", POS_2_X, "|", POS_2_Y);                               //DEBUG
        if(is_closer_to_pos(POS_2_X, POS_2_Y,H2)){
            //other hound takes pos 2
            -+driving_position(4);
            .print("driving_position(4)");                                                                                                  //DEBUG
        }else{
            //I'm closer to pos 2
            -+driving_position(2);
            .print("driving_position(2)");                                                                                                  //DEBUG
        }
    }else{
        // there are two other hounds driving the same swarm
        !updateSwarmData(Swarm);
        ?swarm_data_updated(Swarm, CX,CY, Size, R);
        jia.get_pos_drive_swarm(CX, CY, R, 1, POS_1_X, POS_1_Y);
        for(.member(H3_tmp, Drivers)){                                                                                                          //DEBUG
            ?pos(ME_X, ME_Y);                                                                                                                   //DEBUG
            ?pos_agent(H3X,H3Y)[source(H3_tmp)];                                                                                                //DEBUG
            .print("Pos_ME:", ME_X, "|", ME_Y, " Pos_", H3_tmp, ":", H3X, "|", H3Y, " POS_1:", POS_1_X, "|", POS_1_Y);                           //DEBUG
        }                                                                                                                                       //DEBUG
        if(.member(H3, Drivers) & is_closer_to_pos(POS_1_X, POS_1_Y, H3)){
            //other driving hound is taking pos 1
            jia.get_pos_drive_swarm(CX, CY, R, 3, POS_3_X, POS_3_Y);
            for(.member(H3_tmp2, Drivers)){                                                                                                     //DEBUG
                ?pos(ME_X, ME_Y);                                                                                                               //DEBUG
                ?pos_agent(H3X_2,H3Y_2)[source(H3_tmp2)];                                                                                       //DEBUG
                .print("Pos_ME:", ME_X, "|", ME_Y, " Pos_", H3_tmp2, ":", H3X_2, "|", H3Y_2, " POS_3:", POS_3_X, "|", POS_3_Y);                  //DEBUG
            }  
            if(.member(H4, Drivers) & is_closer_to_pos(POS_3_X, POS_3_Y, H4)){
                //other driving hound is taking pos 3
                -+driving_position(5);
                .print("driving_position(5)");                                                                                              //DEBUG
            }else{
                //I'm closest to pos 3
                -+driving_position(3);
                .print("driving_position(3)");                                                                                              //DEBUG
            }
        }else{
            //I'm closest to pos 1
            -+driving_position(1);
            .print("driving_position(1)");                                                                                                  //DEBUG
        }
    }.


//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("./hound_drive/hound_drive_choose_swarm.asl")}   