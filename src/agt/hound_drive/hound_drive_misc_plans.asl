//------------------------------------------------------- addToSwarm -------------------------------------------------------
+!addToSwarm(Swarm, Sheep)
    <- .set.add(Swarm,Sheep);
    !updateSwarmData(Swarm).

//------------------------------------------------------- guessWhoDrivingWhat -------------------------------------------------------
//TODO change name of plan
  +!guessWhoDrivingWhat
    <- //.print("guessWhoDrivingWhat");                                                                                                                                     //DEBUG
    if(hound_drives(_,_)){
        .abolish(hound_drives(_, _));
    }

    //get all hounds, which positions are known and all known swarms of sheep
    .setof(H, pos_agent(_,_, H) & hound(H) , All_Hounds);
    //.print("All hounds, which position I know: ", All_Hounds);                                                                                                                  //DEBUG
    .findall(Ss, swarm(Ss, _, _, _), Swarms);
    //.print("All Swarms I know: ", Swarms);                                                                                                                                      //DEBUG
    ?limit_distance_assumption_hound_driving(Limit_Distance_Driving);
    for(.member(H_in_focus, All_Hounds)){
        ?pos_agent(HX,HY, H_in_focus);
        for(.member(Ss_2, Swarms)){
            ?swarm(Ss_2, CX_2, CY_2, R_2);
            jia.get_distance(HX,HY,CX_2,CY_2,D_Ss_2);

            //check if hound is within limit to drive, otherwise suspect that hound is not driving the swarm
            if(D_Ss_2 < Limit_Distance_Driving){
                if(not hound_drives(H_in_focus,_)){
                    //.print("hound ", H_in_focus, " drives ", Ss_2);                                                                                                               //DEBUG
                    +hound_drives(H_in_focus, Ss_2);
                }else{
                    // suspect hound to drive the swarm he's closer to
                    ?hound_drives(H_in_focus, Ss_3);
                    ?swarm(Ss_3, CX_3, CY_3, R_3);
                    jia.get_distance(HX,HY,CX_3,CY_3,D_Ss_3);
                    if(D_Ss_2 < D_Ss_3){
                        //.print("hound ", H_in_focus, " drives ", Ss_2);                                                                                                           //DEBUG
                        -+hound_drives(H_in_focus, Ss_2);
                    }
                }
            }
        }
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
    .findall(R, pos_agent(X,Y, S) & .member(S,LS) & jia.get_distance(CX,CY,X,Y,R), List_of_R);   
    R = math.round(math.max(List_of_R));
    .abolish(swarm(LS,_,_,_));
    +swarm(LS, CX, CY, R);
    //.print("Updated swarm: (", LS, ", ", CX, ", ", CY, ", ", R, ")");                                                                     //DEBUG
    .

    //------------------------------------------------------- getQuadrantRadius -------------------------------------------------------

+!getQuadrantRadius(Ss, RQ1, RQ2, RQ3, RQ4)
    <- //.print("getQuadrantRadius(", Ss, ")");
    ?swarm(Ss, Cx, Cy, R);
    //Q1
    .findall(S1, .member(S1, Ss) & pos_agent(S1x, S1y, S1) & S1x >= Cx & S1y <= Cy, Sheep_Q1);
    !getRadiusOfSubSet(Ss, Sheep_Q1, RQ1);
    //Q2
    .findall(S2, .member(S2, Ss) & pos_agent(S2x, S2y, S2) & S2x <= Cx & S2y <= Cy, Sheep_Q2);
    !getRadiusOfSubSet(Ss, Sheep_Q2, RQ2);
    //Q3
    .findall(S3, .member(S3, Ss) & pos_agent(S3x, S3y, S3) & S3x <= Cx & S3y >= Cy, Sheep_Q3);
    !getRadiusOfSubSet(Ss, Sheep_Q3, RQ3);
    //Q4
    .findall(S4, .member(S4, Ss) & pos_agent(S4x, S4y, S4) & S4x >= Cx & S4y >= Cy, Sheep_Q4);
    !getRadiusOfSubSet(Ss, Sheep_Q4, RQ4).
    //.print("Radius per Quadrant Q1:", RQ1, " Q2:", RQ2, " Q3:", RQ3, " Q4:", RQ4).

    //------------------------------------------------------- getRadiusOfSubSet -------------------------------------------------------

+!getRadiusOfSubSet(Ss, Sub_Ss, R)
    <- //.print("getRadius(", Ss, ", ", Sub_Ss, ", R)");
    ?swarm(Ss, Cx, Cy, _);
    .findall(TmpR, .member(S, Sub_Ss) & pos_agent(Sx, Sy, S) & jia.get_distance(Cx,Cy,Sx,Sy,TmpR), ListR);
    .max(ListR, R).
    //.print("getRadius R:", R).