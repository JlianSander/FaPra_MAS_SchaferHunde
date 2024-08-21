
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 
@newPosAgent[atomic]
+pos_agent(X,Y,S) : sheep(S) & is_sheep_of_interest(S) &  situation_ok_to_drive //starts the drive, whenever a sheep was sighted and further criteria are met
    <- 
    //.print("pos_agent new pos accepted for ", S);
    .drop_desire(startSearch);
    .drop_desire(continueSearch);
    .drop_desire(endSearch);
    -is_searching;
    !!startDrive.

is_sheep_of_interest(S) :- ignoredSheep(IgnoredSheep) & not .member(S, IgnoredSheep).

situation_ok_to_drive :- not is_driving & not no_driving.            

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- startSearch -------------------------------------------------------
@startSearch[atomic]
+!startSearch : not is_driving & not is_searching
    <- .print("Search started");
    +is_searching;
    !!endSearch.
    
+!startSearch <- true.

//------------------------------------------------------- endSearch -------------------------------------------------------

+!endSearch
    <- !continueSearch;
    -is_searching;
    !!startSearch.

-!endSearch
    <- .print("endSearch restart search");
    -is_searching;
    !waitToMove;
    !!startSearch.

//------------------------------------------------------- continueSearch -------------------------------------------------------
i_know_sheep_pos :- (pos_agent_old(_, _, S) | pos_agent(_, _, S)) & sheep(S) & is_sheep_of_interest(S) & not no_driving.

+!continueSearch : i_know_sheep_pos 
    <- .print("searchStrategy i_know_sheep_pos");                                                                                                                                   //DEBUG
    .findall(S, pos_agent(_, _, S) & sheep(S) & is_sheep_of_interest(S), List_Sheep);
    .length(List_Sheep, Len_Sheep);
    if(Len_Sheep > 0){
        .nth(0, List_Sheep, S1);
        ?pos_agent(X, Y, S1);
        !goToSheep(S1, X, Y);
    }else{
        .findall(S, pos_agent_old(_, _, S) & sheep(S) & is_sheep_of_interest(S), List_Sheep_old);
        .nth(0, List_Sheep_old, S1);
        ?pos_agent_old(X, Y, S1);
        !goToSheep(S1, X, Y);
    }
    .

+!goToSheep(S1, X, Y) 
    <- .print("GoToSheep(", S1, ",", X, ",", Y, ")" );
    ?pos(MyX, MyY);
    .print("Search for sheep at (", X, ",", Y, ") while I'm at (", MyX, ",", MyY, ")");
    if(pos(X, Y)){ //hound stands on same spot
        !searchWithout(S1);
    }else{
        ?pos(Xme, Yme);
        jia.hounds.get_next_pos(Xme, Yme, 0, 0, X, Y, XNext, YNext);
        .print("continueSearch myLoc (", Xme, ",", Yme, ") Target:(", X, ",", Y, ") Next:(", XNext, ",", YNext, ")");
        if(XNext == Xme & YNext == Yme){
            //can't reach that sheep
            !searchWithout(S1);
        }else{
            !reachDestination(XNext,YNext);
            !continueSearch;
        }
    }.

 +!continueSearch : not i_know_sheep_pos
    <- .print("continue Search");
    !selectSearchStrategy;
    .

//------------------------------------------------------- searchWithout -------------------------------------------------------
+!searchWithout(S) 
    <- .print("searchWithout ", S);
    .set.create(LS);
    .set.add(LS, S);
    .print("searchWithout add to ignore ", LS);
    !ignoreSheep(LS);
    !!forgetIgnoreSheep(LS);
    !waitToMove;
    !continueSearch;
    .

//------------------------------------------------------- selectSearchStrategy -------------------------------------------------------
+!selectSearchStrategy 
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
//------------------------------------------------------- searchStrategy1 -------------------------------------------------------
+!searchStrategy1
    <-
    jia.hounds.get_random_position(TargetX, TargetY);
    !reachDestination(TargetX, TargetY);
    !continueSearch;
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
        !continueSearch;
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
        !continueSearch;
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
    }elif(XSearchPos == Xme & YSearchPos == Yme){
        //the next pos in the pattern is not reachable
        ?search_pattern(X, Y, I, IsInverse, _, _);
        -+search_pattern(X, Y, I, IsInverse, -1, -1);       
        !proceedSearchStrat2;
    }else{
        !proceedSearchStrat2;
    }
    .