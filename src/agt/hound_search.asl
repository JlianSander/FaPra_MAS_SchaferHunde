
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
    !!setMove;
    .

+!setMove : not .desire(reachDestination(L,M))
    <-
    jia.get_random_position(TargetX, TargetY);
    !reachDestination(TargetX, TargetY);
    !!setMove;
    .

+!setMove <- true.