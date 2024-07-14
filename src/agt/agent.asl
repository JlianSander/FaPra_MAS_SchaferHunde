//////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////
!start.

all_hound_names([]).
all_sheep_names([]).

+!start
    <-
        .my_name(Me);
        initAgent(Me, AgX, AgY);
        jia.util.common.init_agent;
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

//------------------------------------------------------- Send to agent list -------------------------------------------------------

+!sendToListOfAgents([], Atom, Msg).
+!sendToListOfAgents([X|Ls], Atom, Msg)
    <- .send(X, Atom, Msg);
       !sendToListOfAgents(Ls, Atom, Msg).

//------------------------------------------------------- updatePos -------------------------------------------------------   

+!updatePos(X,Y) : not last_step_not_OK <-  -+pos(X, Y).

+!updatePos(X,Y) : last_step_not_OK <- -last_step_not_OK; .print("last step not ok").


//------------------------------------------------------- waitToMove -------------------------------------------------------   

+!waitToMove : waitTime(Wait) <- .wait(Wait).

//////////////////////////////////////////////////////////////////////////////////////////////////// Includes ////////////////////////////////////////////////////////////////////////////////////////////////////

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }