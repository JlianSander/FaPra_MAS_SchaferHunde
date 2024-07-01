package jia.util;

import java.util.List;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;

public class DriveStrategyCommon {
    
     public static Location calculatePosition(TransitionSystem ts, int positionNumber, Double angle_incr, RealVector directionFromCenter, int radiusSwarm, int offsetRadius, Location center, GridModel model)
     throws ExceptionPositioningFailed{
        double angle = calcAngle(ts, positionNumber, angle_incr);
       
        Location agentPos = calcPosOnCircle(ts, center, directionFromCenter, angle, radiusSwarm + offsetRadius + 1);

        return ensurePosValid(ts, center, model, agentPos);
     }

     public static double calcAngle(TransitionSystem ts, int positionNumber, Double angle_incr) {
        //calculate angle depending on the positionNumber
        double angle = (3 - positionNumber) * angle_incr;
        ts.getLogger().info("--------------'DriveStrategyCommon::calcAngle' angle: " + angle);                                                                             //DEBUG
        return angle;
    }

    public static Location calcPosOnCircle(TransitionSystem ts, Location center, RealVector directionFromCenter, double angleInDegree, int radius){
        RealVector directionSwarmRotated = GeometryCalculator.translateAngle(ts, directionFromCenter, angleInDegree);
        ts.getLogger().info("--------------'DriveStrategyCommon::positionAgent' directionSwarmRotated: " + directionSwarmRotated.toString());                 //DEBUG

        //calculate agents position behind the swarm
        var agentPos = GeometryCalculator.translateInDir(ts, center, directionSwarmRotated, radius);
        ts.getLogger().info("--------------'DriveStrategyCommon::positionAgent' agentPos: " + agentPos.toString());                 //DEBUG
        return agentPos;
    }
    
    private static Location ensurePosOnMap(TransitionSystem ts, GridModel model, Location targetPos) {
        //ts.getLogger().info("--------------'DriveStrategyCommon::ensurePosOnMap' targetPos: " + targetPos.toString());                                    //DEBUG
        //ensure to stay on map
        Location validPos = new Location(
                IDrivePositioner.stay_within_limit(ts, targetPos.x, model.getWidth() - 1),
                IDrivePositioner.stay_within_limit(ts, targetPos.y, model.getHeight() - 1));

        ts.getLogger().info("--------------'DriveStrategyCommon::ensurePosOnMap' validPos: " + validPos.toString());                                    //DEBUG
        return validPos;
    }

    public static Location ensurePosValid(TransitionSystem ts, Location center, GridModel model, Location targetPos)
    throws ExceptionPositioningFailed {
        Location validPos = ensurePosOnMap(ts, model, targetPos);

        //prevent to position agent on spot of swarm center in case the position got changed because of map limits
        if (validPos.equals(center)) {
            if (validPos.x != model.getWidth() - 1) {
                validPos = new Location(validPos.x + 1, validPos.y);
            } else if (validPos.y != model.getHeight() - 1) {
                validPos = new Location(validPos.x, validPos.y + 1);
            } else {
                validPos = new Location(validPos.x - 1, validPos.y - 1);
            }
        }

        List<Integer> atPos = model.getObjectsAt(validPos);

        if(atPos.contains(GridModel.OBSTACLE)){
            throw new ExceptionPositioningFailed("Target position is obstacle");
        }

        ts.getLogger().info("--------------'DriveStrategyCommon::ensurePosValid' validPos: " + validPos.toString());                                    //DEBUG

        return validPos;
    }
}
