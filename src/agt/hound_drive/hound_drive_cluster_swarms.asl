//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 



//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- clusterSwarms -------------------------------------------------------

+!clusterSwarms
    <- //.print("clusterSwarms");                                                                                                               //DEBUG
    if(swarm(_, _, _, _, _)){
        .abolish(swarm(_, _, _, _, _));  
    }

    !startClusterStrat1.
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
//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////
{ include("./hound_drive/clustering_strategy_1.asl")}