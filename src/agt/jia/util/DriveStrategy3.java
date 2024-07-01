package jia.util;

import java.security.InvalidAlgorithmParameterException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;

import jason.environment.grid.Area;
import jason.environment.grid.Location;
import util.PropertiesLoader;

public class DriveStrategy3 implements IDrivePositioner{

    @Override
    public Location calculateAgentPosition(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber)
            throws ExceptionPositioningFailed {
         //ts.getLogger().info("--------------'positionAgent' positionNumber: " + positionNumber);                                                                       //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer houndDistanceToSwarm = loader.getProperty("hound_keep_distance_to_swarm", Integer.class);
        Double angleIncr = loader.getProperty("hound_driving_position_angle_increment", Double.class);
        //ts.getLogger().info("--------------'positionAgent' angle_incr: " + angle_incr);                                                                               //DEBUG

        RealVector directionSwarm = swarm.getDirectionTo(ts, corral.center());
        RealVector invertedDirection = directionSwarm.mapMultiply(-1.0);

        var lstPosSwarm = BeliefBaseManager.getPosOfSheep(ts, swarm);
        
        if(positionNumber != 3){
            return calcTranslatedAgtPos(ts, swarm, positionNumber, model, houndDistanceToSwarm, angleIncr, directionSwarm,
                invertedDirection, lstPosSwarm);
        }else{
            return calcCenterPos(ts, swarm, model, houndDistanceToSwarm,  
                invertedDirection, lstPosSwarm);
        }
    }

    private Location calcCenterPos(TransitionSystem ts, SwarmManipulator swarm, GridModel model, Integer houndDistanceToSwarm, 
        RealVector invertedDirection, ArrayList<Location> lstPosSwarm) throws ExceptionPositioningFailed{
        var posAgent = calcPosBehindSheep(ts, swarm, houndDistanceToSwarm, lstPosSwarm, invertedDirection);
        return DriveStrategyCommon.ensurePosValid(swarm.getCenter(), model, posAgent);
    }

    private Location calcTranslatedAgtPos(TransitionSystem ts, SwarmManipulator swarm, int positionNumber, GridModel model,
            Integer houndDistanceToSwarm, Double angleIncr, RealVector directionSwarm, RealVector invertedDirection,
            ArrayList<Location> lstPosSwarm) throws ExceptionPositioningFailed {
        //get the sheep, which is positioned farest away from the center in a direction translated by +90° of the direction of the swarm
        RealVector dir1Perp = GeometryCalculator.translateAngle(ts, directionSwarm, 90.0);
        var p1 = calcPosBehindSheep(ts, swarm, houndDistanceToSwarm, lstPosSwarm, dir1Perp);
        //create a direction, that is angular translated by 90° to the direction of interest
        RealVector dir1 = directionSwarm;
        

        double angle = DriveStrategyCommon.calcAngle(positionNumber, angleIncr);
        //get the sheep, which is positioned farest away from the center in a direction translated by angle° of the inverted direction of the swarm
        RealVector dir2Perp = GeometryCalculator.translateAngle(ts, invertedDirection, angle);
        var p2 = calcPosBehindSheep(ts, swarm, houndDistanceToSwarm, lstPosSwarm, dir2Perp);
        //create a direction, that is angular translated by 90° to the direction of interest
        RealVector dir2 = GeometryCalculator.translateAngle(ts, dir2Perp, 90.0);

        Location agentPos;
        try{
            agentPos = GeometryCalculator.calcStraightIntersection(p1, dir1, p2, dir2);
        }catch(InvalidAlgorithmParameterException e){
            //should not happen since directions cant be parallel to each other
            throw new RuntimeException(e);
        }
        return DriveStrategyCommon.ensurePosValid(swarm.getCenter(), model, agentPos);
    }

    private Location calcPosBehindSheep(TransitionSystem ts, SwarmManipulator swarm, Integer houndDistanceToSwarm,
            ArrayList<Location> lstPosSwarm, RealVector directionFromCenter) {
        //get the sheep, which is positioned farest away from the center in the specified direction
        CompDistanceInDir comp2 = new CompDistanceInDir(directionFromCenter, swarm.getCenter());
        var sheep2 = Collections.max(lstPosSwarm, comp2);
        //create a location close to the position of the sheep, but with an offset in the direction of interest
        var p2 = GeometryCalculator.translateInDir(ts, sheep2, directionFromCenter, houndDistanceToSwarm);
        return p2;
    }

    class CompDistanceInDir implements Comparator<Location>{

        private RealVector direction;
        private Location origin;

        public CompDistanceInDir(RealVector direction, Location origin) {
            this.direction = direction;
            this.origin = origin;
        }

        @Override
        public int compare(Location o1, Location o2) {
            double dist1 = GeometryCalculator.calcDistanceInDir(origin, o2, direction);
            double dist2 = GeometryCalculator.calcDistanceInDir(origin, o2, direction);

            if(dist1 == dist2) return 0;
            return dist1 > dist2 ? 1 : -1;
        }

    }
}
