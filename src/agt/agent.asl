//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////
!start.

repeated_pos(0).

all_hound_names([]).
all_sheep_names([]).

+!start
    <-
        .my_name(Me);
        initAgent(Me, AgX, AgY);
        jia.util.init_agent;
        +pos(AgX,AgY);
        ?waitTime(Wait);
        !prepareAgentLists;
        .print("Done agent init of: ", Me, " at (", AgX, ",", AgY, ") with wait time: ", Wait);
        !init.

//------------------------------------------------------- Prepare agent lists-------------------------------------------------------

+!prepareAgentLists
    <-
        .all_names(AA);
        !filterNames(AA, "hound", AH);
        !filterNames(AA, "sheep", AS);
        -+all_hound_names(AH);
        -+all_sheep(AS).

+!filterNames(Names, SubStr, FilteredNames)
<- .findall(X, (.member(X, Names) & .term2string(Y, X) & .lower_case(Y, Q) & .substring(SubStr, Q)), FilteredNames).

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////

+pos(X,Y) <- 
    !broadCastPos(X,Y).

//------------------------------------------------------- Send to agent list -------------------------------------------------------

+!sendToListOfAgents([], Atom, Msg).
+!sendToListOfAgents([X|Ls], Atom, Msg)
    <- .send(X, Atom, Msg);
       !sendToListOfAgents(Ls, Atom, Msg).

//------------------------------------------------------- broadcastPos -------------------------------------------------------

+!broadCastPos(X,Y) : all_hound_names(AH)
    <- .my_name(ID);
        // .broadcast(achieve, trackMove(X,Y));
        !sendToListOfAgents(AH, achieve, trackMove(X,Y));
        .print(ID," moves to: (", X," , ", Y, ")").

//------------------------------------------------------- updatePos -------------------------------------------------------   

+!updatePos(X,Y) : not last_step_not_OK <-  -+pos(X, Y).

+!updatePos(X,Y) : last_step_not_OK <- -last_step_not_OK; .print("last step not ok").


//------------------------------------------------------- waitToMove -------------------------------------------------------   

+!waitToMove : waitTime(Wait) <- .wait(Wait).

//------------------------------------------------------- repeatPos -------------------------------------------------------  
//DEBUG: this plan should become obsolete, once the agents can actively perceive the environment they move to
// needed to tell hounds position of non-moving sheep
+!repeatPos : not pos(X,Y)
    <- .wait(1000);
    !!repeatPos.

+!repeatPos : pos(X,Y) & repeated_pos(I) & I < 1
    <- !broadCastPos(X,Y);
    -+repeated_pos(I + 1);
    .wait(1000);
    !!repeatPos.

+!repeatPos <- true.

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }