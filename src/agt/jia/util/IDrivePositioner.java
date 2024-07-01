package jia.util;

import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;
import jason.environment.grid.Location;

public interface IDrivePositioner {
     public Location calculateAgentPosition(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber) throws ExceptionPositioningFailed;

     public static int stay_within_limit(TransitionSystem ts, int actVal, int maxLimit) {
          //ts.getLogger().info("--------------'IDrivePositioner::stay_within_limit' actVal: " + actVal);                                    //DEBUG
          //ts.getLogger().info("--------------'IDrivePositioner::stay_within_limit' maxLimit: " + maxLimit);                                    //DEBUG
          int output = actVal > 0 ? actVal : 0;
          output =  output < maxLimit ? output : maxLimit;
          //ts.getLogger().info("--------------'IDrivePositioner::stay_within_limit' output: " + output);                                    //DEBUG
          return output;
      }
}
