
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 

+pos_agent(X,Y,S) : sheep(S) & is_sheep_of_interest(S) &  situation_ok_to_drive //starts the drive, whenever a sheep was sighted and further criteria are met
    <- 
    .drop_desire(startSearch);
    !!startDrive.

is_sheep_of_interest(S) :- ignoredSheep(IgnoredSheep) & not .member(S, IgnoredSheep).

situation_ok_to_drive :- not is_driving.            

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////
@startSearch[atomic]
+!startSearch : not is_driving & not .desire(selectSearchStrategy)
    <- .print("Search started");
    !!selectSearchStrategy.

+!startSearch <- true.

//------------------------------------------------------- selectSearchStrategy -------------------------------------------------------
i_know_sheep_pos :- pos_agent(_, _, S) & sheep(S) & is_sheep_of_interest(S).

+!selectSearchStrategy : i_know_sheep_pos 
    <- //.print("searchStrategy2 i_know_sheep_pos");                                                                                                                                   //DEBUG
    .findall(S, pos_agent(_, _, S) & sheep(S) & is_sheep_of_interest(S), List_Sheep);
    .nth(0, List_Sheep, S1);
    ?pos_agent(X, Y, S1);
    if(pos(X, Y)){ //hound stands on same spot
        .print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!standing at same spot");
        .abolish(pos_agent(X, Y, S1));
        !waitToMove;
        !!startSearch;
    }else{
        ?pos(Xme, Yme);
        jia.hounds.get_next_pos(Xme, Yme, 0, 0, X, Y, XNext, YNext);
        .print("selectSearchStrategy myLoc (", Xme, ",", Yme, ") Target:(", X, ",", Y, ") Next:(", XNext, ",", YNext, ")");
        if(XNext == Xme & YNext == Xme){
            //can't reach that sheep
            .abolish(pos_agent(X, Y, S1));
            !waitToMove;
        }else{
            !reachDestination(XNext,YNext);
        }
    !!startSearch;
    }
    .

+!selectSearchStrategy : not i_know_sheep_pos
    <- .print("selectSearchStrategy");
    ?search_strategy(X);
    if(X == 1){
        !searchStrategy1;
    }elif(X == 2){
        !searchStrategy2;
    }else{
        .print("ERROR no such search strategy!");
    }
    .

-!selectSearchStrategy 
    <- .print("selectSearchStrategy   failed");
    !waitToMove;
    !!startSearch.

//------------------------------------------------------- searchStrategy1 -------------------------------------------------------
+!searchStrategy1
    <-
    jia.hounds.get_random_position(TargetX, TargetY);
    !reachDestination(TargetX, TargetY);
    !!startSearch;
    .

//------------------------------------------------------- searchStrategy2 -------------------------------------------------------

+!searchStrategy2 : not search_pattern(_, _, _, _, _, _)
    <- //.print("searchStrategy2 I");                                                                                                                                   //DEBUG
    jia.hounds.get_search_area(X,Y);
    //.print("searchStrategy2 search middle point: (", X, ",", Y, ")");                                                                                                 //DEBUG
    +search_pattern(X, Y, 0, 0, -1, -1);
    !proceedSearchStrat2;
    .

+!searchStrategy2 : search_pattern(Xq, Yq, I, IsInverse, XSearchPos, YSearchPos)
    <- //.print("searchStrategy2 II");                                                                                                                                   //DEBUG    
    //check if search area stays the same
    if(jia.hounds.check_search_area(Xq, Yq)){
        //.print("searchStrategy2 II - continue pattern");                                                                                                                //DEBUG
        ?pos(Xme, Yme);
        if(XSearchPos == Xme & YSearchPos == Yme){
            -+search_pattern(Xq, Yq, I, IsInverse, -1, -1);
        }
        !proceedSearchStrat2;
    }else{
        //.print("searchStrategy2 II - reset search pattern");                                                                                                               //DEBUG
        .abolish(search_pattern(_, _, _, _, _, _));
        !waitToMove;
        !!startSearch;
    }
    .

//------------------------------------------------------- proceedSearchStrat2 -------------------------------------------------------    

+!proceedSearchStrat2 : search_pattern(X, Y, I, IsInverse, XSearchPos, YSearchPos) & XSearchPos \== -1 & YSearchPos \== -1  // plan to reach calculated pos in search pattern
    <- //.print("proceedSearchStrat2  I Middle-Point:(", X, ",", Y , ")");                                                                                                                                   //DEBUG
    ?pos(Xme, Yme);
    jia.hounds.get_next_pos_no_check(XSearchPos, YSearchPos, XNext, YNext);
    //.print("My Pos: ", Xme, ",", Yme, " Search Pos: ", XSearchPos, ",", YSearchPos , ", Next Step to Pos ", XNext, ",", YNext);                                   //DEBUG
    if(Xme == XNext & Yme == YNext){
        //can't reach desired target 
        -+search_pattern(X, Y, I, IsInverse, -1, -1);
        !proceedSearchStrat2;
    }else{
        //make one step
        !reachDestination(XNext, YNext);
        //reassess situation after one step
        !!startSearch;
    }
    .

+!proceedSearchStrat2 : search_pattern(X, Y, I, IsInverse, -1, -1)              // plan to calculate new position in search pattern
    <- //.print("proceedSearchStrat2  II Middle-Point:(", X, ",", Y , ")");                                                                                                                                   //DEBUG
    ?search_space_between_lines(Space);
    jia.hounds.get_next_search_pos(I, X, Y, Space, IsInverse, XSearchPos, YSearchPos, Itrs);
    -+search_pattern(X, Y, Itrs, IsInverse, XSearchPos, YSearchPos);
    ?pos(Xme, Yme);
    if(XSearchPos == -1 & YSearchPos == -1){
        //has finished search pattern
        .abolish(search_pattern(_, _, _, _, _, _));
        !waitToMove;
        !!startSearch;
    }elif(XSearchPos == Xme & YSearchPos == Yme){
        //the next pos in the pattern is not reachable
        ?search_pattern(X, Y, I, IsInverse, _, _);
        -+search_pattern(X, Y, I, IsInverse, -1, -1);       
        !proceedSearchStrat2;
    }else{
        !proceedSearchStrat2;
    }
    .