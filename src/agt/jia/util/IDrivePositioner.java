package jia.util;

import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;
import jason.environment.grid.Location;

public interface IDrivePositioner {
     public Location calculateAgentPosition(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber) throws ExceptionPositioningFailed;

     public static int stay_within_limit(int actVal, int maxLimit) {
          int output = actVal > 0 ? actVal : 0;
          return output < maxLimit ? output : maxLimit;
      }
}
