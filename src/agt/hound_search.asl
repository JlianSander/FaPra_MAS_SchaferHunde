
//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs //////////////////////////////////////////////////////////////////////////////////////////////////// 

+pos_agent(X,Y,S) : sheep(S) & is_sheep_of_interest(S) &  situation_ok_to_drive //starts the drive, whenever a sheep was sighted and further criteria are met
    <- 
    .drop_desire(walkTowards(_,_));
    .drop_desire(reachDestination(_,_));
    !!startDrive.  

is_sheep_of_interest(S) :- ignoredSheep(IgnoredSheep) & not .member(S, IgnoredSheep).

situation_ok_to_drive :- not is_driving.   //TODO in situation_ok_to_drive können weitere Kriterien z.B. über eine jia definiert werden um die Situation näher zu untersuchen               

// situation_ok_to_drive :- 
//     .findall(S, sheep(S), Ss) & .length(Ss, Len_Ss) & Len_Ss > 3.        //starts driving if the positions of more than 3 sheep are known 
//situation_ok_to_drive:- jia.check_nearby_sheep.


//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////
+!startSearch
    <- .print("Search started");
    ?search_strategy(X);
    if(X == 1){
        !!searchStrategy1;
    }elif(X == 2){
        !!searchStrategy2;
    }else{
        .print("ERROR no such search strategy!");
    }
    .

//------------------------------------------------------- searchStrategy1 -------------------------------------------------------
+!searchStrategy1 : not .desire(reachDestination(L,M))
    <-
    jia.get_random_position(TargetX, TargetY);
    !reachDestination(TargetX, TargetY);
    !!searchStrategy1;
    .

+!searchStrategy1 <- true.

-!searchStrategy1 <- !!searchStrategy1.

//------------------------------------------------------- searchStrategy2 -------------------------------------------------------

i_know_sheep_pos :- pos_agent(_, _, S) & sheep(S) & is_sheep_of_interest(S).

+!searchStrategy2 : i_know_sheep_pos 
    <- .print("searchStrategy2 i_know_sheep_pos");
    .findall(S, pos_agent(_, _, S) & sheep(S) & is_sheep_of_interest(S), List_Sheep);
    .nth(0, List_Sheep, S1);
    ?pos_agent(X, Y, S1);
    !reachDestination(X,Y);
    !!startSearch.

+!searchStrategy2 : not i_know_sheep_pos & not search_pattern(_, _, _, _)
    <- .print("searchStrategy2");
    jia.get_search_area(X,Y);
    .print("searchStrategy2 search middle point: (", X, ",", Y, ")");
    +search_pattern(X, Y, 0, 0);
    !proceedSearchStrat2.

+!searchStrategy2 : not i_know_sheep_pos & search_pattern(X, Y, I, IsInverse)
    <- .print("searchStrategy2");    
    !proceedSearchStrat2.

+!proceedSearchStrat2 : search_pattern(X, Y, I, IsInverse)
    <- jia.get_next_search_pos(I, X, Y, 1, IsInverse, Xnext, Ynext, Itrs);
    -+search_iterations(X, Y, Itrs, IsInverse);
    ?pos(Xme, Yme);
    if(Xnext == Xme & Ynext == Yme){
        .print("searchStrategy2   !!!!!!!!!!!! stuck");
        if(search_stucked(J) & J > 10){
            .abolish(search_iterations(_, _, _, _));
        }elif(search_stucked(J) & J < 10){
            -+search_stucked(J + 1);
            .wait(500);
        }else{
            +search_stucked(1);
            .wait(500);
        }
        !!startSearch;
    }else{
        !reachDestination(Xnext, Ynext);
        !!startSearch;
    }
    .