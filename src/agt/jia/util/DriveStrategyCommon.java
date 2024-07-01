package jia.util;

import java.util.List;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;

public class DriveStrategyCommon {
    
     public static Location calculatePosition(TransitionSystem ts, int positionNumber, Double angle_incr, RealVector directionFromCenter, int radius, int offset_radius, Location center, GridModel model)
     throws ExceptionPositioningFailed{
        double angle = calcAngle(positionNumber, angle_incr);
       
        Location agentPos = calcPosOnCircle(ts, center, directionFromCenter, angle, radius + offset_radius);

        return ensurePosValid(center, model, agentPos);
     }

     public static double calcAngle(int positionNumber, Double angle_incr) {
        //calculate angle depending on the positionNumber
        double angle = (3 - positionNumber) * angle_incr;
        //ts.getLogger().info("--------------'positionAgent' calculated angle: " + angle);                                                                             //DEBUG
        return angle;
    }

    public static Location calcPosOnCircle(TransitionSystem ts, Location center, RealVector directionFromCenter, double angleInDegree, int radius){
        RealVector direction_swarm_rotated = GeometryCalculator.translateAngle(ts, directionFromCenter, angleInDegree);
        //ts.getLogger().info("--------------'positionAgent' swarm_direction rotated: [" + direction_swarm_rotated.getEntry(0) + "][" + direction_swarm_rotated.getEntry(1) + "]");                 //DEBUG

        //calculate agents position behind the swarm
        var agentPos = GeometryCalculator.translateInDir(ts, center, direction_swarm_rotated, radius);
        //ts.getLogger().info("--------------'positionAgent' agent_pos before map limit: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG
        return agentPos;
    }
    
    private static Location ensurePosOnMap(GridModel model, Location agentPos) {
        //ensure to stay on map
        agentPos = new Location(
                IDrivePositioner.stay_within_limit(agentPos.x, model.getWidth() - 1),
                IDrivePositioner.stay_within_limit(agentPos.y, model.getHeight() - 1));

        //ts.getLogger().info("--------------'positionAgent' agent_pos after map limit: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG
        return agentPos;
    }

    public static Location ensurePosValid(Location center, GridModel model, Location agentPos)
    throws ExceptionPositioningFailed {
        agentPos = ensurePosOnMap(model, agentPos);

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
