package jia.util;

import java.util.List;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;

public class DriveStrategyCommon {
    
     public static Location calculatePosition(TransitionSystem ts, int positionNumber, Double angle_incr, RealVector directionFromCenter, int radius, int offset_radius, Location center, GridModel model)
     throws ExceptionPositioningFailed{
        //calculate angle depending on the positionNumber
        double angle = (3 - positionNumber) * angle_incr;
        //ts.getLogger().info("--------------'positionAgent' calculated angle: " + angle);                                                                             //DEBUG
       
        Location agentPos = AngleTranslation.translate(ts, center, directionFromCenter, angle, radius + offset_radius);

        //ensure to stay on map
        agentPos = new Location(
                IDrivePositioner.stay_within_limit(agentPos.x, model.getWidth() - 1),
                IDrivePositioner.stay_within_limit(agentPos.y, model.getHeight() - 1));

        //ts.getLogger().info("--------------'positionAgent' agent_pos after map limit: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG

        //prevent to position agent on spot of swarm center in case the position got changed because of map limits
        if (agentPos.equals(center)) {
            if (agentPos.x != model.getWidth() - 1) {
                agentPos = new Location(agentPos.x + 1, agentPos.y);
            } else if (agentPos.y != model.getHeight() - 1) {
                agentPos = new Location(agentPos.x, agentPos.y + 1);
            } else {
                agentPos = new Location(agentPos.x - 1, agentPos.y - 1);
            }
        }

        List<Integer> atPos = model.getObjectsAt(agentPos);

        if(atPos.contains(GridModel.OBSTACLE)){
            throw new ExceptionPositioningFailed("Target position is obstacle");
        }

        //ts.getLogger().info("--------------'positionAgent' agent_pos final: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG

        return agentPos;
     }
}
