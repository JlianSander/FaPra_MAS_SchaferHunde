////////////////////////////////////////////////////////////////////////////////////////////////// Init ////////////////////////////////////////////////////////////////////////////////////////////////////

// No special initialization required

//////////////////////////////////////////////////////////////////////////////////////////////////// Beliefs ////////////////////////////////////////////////////////////////////////////////////////////////////    

// No specific beliefs required

//////////////////////////////////////////////////////////////////////////////////////////////////// Plans ////////////////////////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------- searchSheep -------------------------------------------------------

+!search_sheep <- .print("Searching for sheep flock.");
                  !random_walk. //Später get_nearest_sheep_in_range

//------------------------------------------------------- random_walk -------------------------------------------------------

+!random_walk <- .print("Randomly walking to search for sheep.");
                  ?pos(AgX, AgY);
                  !get_random_position(NewX, NewY); //AgX wird zu NewX, AgY => NewY
                  .print("Walking to random position: (", NewX, ",", NewY, ")");
                  !reachDestination(NewX, NewY).

//------------------------------------------------------- reachDestination -------------------------------------------------------

+!reachDestination(X, Y) : not pos(X, Y) // not yet reached target coordinates
    <- .print("walking towards: (", X, ",", Y, ")");
       !makeStepTowards(X, Y);
       .wait(100);
       !reachDestination(X, Y).

//+!reachDestination(X, Y) <- .print("reached destination, continuing search."); // reached target coordinates
                             !random_walk.

//////////////////////////////////////////////////////////////////////////////////////////////////// Helper Actions ////////////////////////////////////////////////////////////////////////////////////////////////////

+!makeStepTowards(X, Y) <- nextStep(X, Y, NewX, NewY); !updatePos(NewX, NewY). //nextSteph zum Updaten neuer Position

//+!updatePos(X, Y) : not last_step_not_OK <- -+pos(X, Y).  //Abfangbedingung

//+!updatePos(X, Y) : last_step_not_OK <- -last_step_not_OK; .print("last step not ok"). //Abfangbedingung

+!get_random_position(X, Y) <- jia.random_position(X, Y). // Versuch java random_position

/* For Check
BDI

Inspection of agent houndX (cycle #12247) 
 - Beliefs
 
corral_area(8,8,11,11)[source(self)]
focusing(cobj_8,animals,"ora4mas.nopl.OrgBoard",cobj_2,animals,"/main/animals")[artifact_id(cobj_7),artifact_name(body_houndX),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
focusing(cobj_6,hounds,"ora4mas.nopl.GroupBoard",cobj_2,animals,"/main/animals")[artifact_id(cobj_7),artifact_name(body_houndX),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
focusing(cobj_4,grid,"grid.GridWorld",cobj_3,world,"/main/world")[artifact_id(cobj_5),artifact_name(body_houndX),percept_type(obs_prop),source(percept),workspace("/main/world",cobj_3)]
formationStatus(ok)[artifact_id(cobj_6),artifact_name(hounds),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
group(hounds,hounds_specs,cobj_6)[artifact_id(cobj_8),artifact_name(animals),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
in_sight(X,Y)[source(self)] :- 

(pos(AgX,AgY) & jia.in_line_of_sight(AgX,AgY,X,Y,7))
is_jammed[source(self)] :- 

(jammed(J) & (J > 10))
jammed(0)[source(self)]
joinedWsp(cobj_3,world,"/main/world")[artifact_id(cobj_1),artifact_name(session_houndX),percept_type(obs_prop),source(percept),workspace("/main",cobj_0)]
joinedWsp(cobj_2,animals,"/main/animals")[artifact_id(cobj_1),artifact_name(session_houndX),percept_type(obs_prop),source(percept),workspace("/main",cobj_0)]
joinedWsp(cobj_0,main,"/main")[artifact_id(cobj_1),artifact_name(session_houndX),percept_type(obs_prop),source(percept),workspace("/main",cobj_0)]
parentGroup(root)[artifact_id(cobj_6),artifact_name(hounds),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
play(houndX,driver,hounds)[artifact_id(cobj_6),artifact_name(hounds),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
pos(3,3)[source(self)]
pos_agent(5,10)[source(sheep2)]
pos_agent(10,10)[source(sheep5)]
pos_agent(2,10)[source(sheep1)]
pos_agent(10,2)[source(sheep3),source(sheep4),source(sheep6),source(sheep7)]
schemes([])[artifact_id(cobj_6),artifact_name(hounds),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
sheep(sheep5)[source(sheep5)]
sheep(sheep6)[source(sheep6)]
sheep(sheep7)[source(sheep7)]
sheep(sheep1)[source(sheep1)]
sheep(sheep3)[source(sheep3)]
sheep(sheep2)[source(sheep2)]
sheep(sheep4)[source(sheep4)]
specification(os(mas_schafherde,group_specification(hounds_specs,[role(driver,[],[soc],1,99,[],[])],[],properties([])),[],[]))[artifact_id(cobj_8),artifact_name(animals),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
specification(group_specification(hounds_specs,[role(driver,[],[soc],1,99,[],[])],[],properties([])))[artifact_id(cobj_6),artifact_name(hounds),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
subgroups([])[artifact_id(cobj_6),artifact_name(hounds),percept_type(obs_prop),source(percept),workspace("/main/animals",cobj_2)]
 - Events
 
Sel
Trigger
Intention
X
+!trackMove(12,18)[source(sheep7)]
*/