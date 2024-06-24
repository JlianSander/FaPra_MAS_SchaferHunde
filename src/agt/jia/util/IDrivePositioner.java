package jia.util;

import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;
import jason.environment.grid.Location;

public interface IDrivePositioner {
     public Location calculateAgentPosition(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber) throws ExceptionPositioningFailed;
}
