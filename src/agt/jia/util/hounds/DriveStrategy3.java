package jia.util.hounds;

import java.security.InvalidAlgorithmParameterException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.NoSuchElementException;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;

import jason.environment.grid.Area;
import jason.environment.grid.Location;
import jia.util.common.BeliefBaseManager;
import jia.util.common.GeometryCalculator;
import util.PropertiesLoader;

public class DriveStrategy3 implements IDrivePositioner {

    @Override
    public Location calculateAgentPosition(TransitionSystem ts, Location myLoc, SwarmManipulator swarm, Area corral,
            int positionNumber) {
        ts.getLogger().info("--------------'DriveStrategy3::calculateAgentPosition' positionNumber: " + positionNumber);                                                                             //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer houndDistanceToSwarm = loader.getProperty("hound_keep_distance_to_sheep", Integer.class);
        Double angleIncr = loader.getProperty("hound_drive_angle_increment", Double.class);
        ts.getLogger().info("--------------'DriveStrategy3::calculateAgentPosition' angleIncr: " + angleIncr.toString());                                                                            //DEBUG

        RealVector directionSwarm = swarm.getDirectionTo(ts, corral.center());
        ts.getLogger().info("--------------'DriveStrategy3::calculateAgentPosition' directionSwarm: " + directionSwarm.toString());                                                                  //DEBUG
        RealVector invertedDirection = directionSwarm.mapMultiply(-1.0);
        ts.getLogger().info("--------------'DriveStrategy3::calculateAgentPosition' invertedDirection: " + invertedDirection.toString());                                                            //DEBUG

        var lstPosSwarm = BeliefBaseManager.getPosOfSheep(ts, swarm);
        ts.getLogger().info("--------------'DriveStrategy3::calculateAgentPosition' lstPosSwarm: " + lstPosSwarm.toString());                                                                        //DEBUG

        if (positionNumber != 3) {
            return calcTranslatedAgtPos(ts, myLoc, swarm, positionNumber, model, houndDistanceToSwarm, angleIncr,
                    directionSwarm,
                    invertedDirection, lstPosSwarm);
        } else {
            return calcCenterPos(ts, myLoc, swarm, model, houndDistanceToSwarm,
                    invertedDirection, lstPosSwarm);
        }
    }

    private Location calcCenterPos(TransitionSystem ts, Location myLoc, SwarmManipulator swarm, GridModel model,
            Integer offsetToSheep,
            RealVector invertedDirection, ArrayList<Location> lstPosSwarm) {
        var posAgent = calcPosBehindSheep(ts, swarm, offsetToSheep + 1, lstPosSwarm, invertedDirection);
        ts.getLogger().info("--------------'DriveStrategy3::calcCenterPos' posAgent: " + posAgent.toString());                                                                               //DEBUG
        Location afterEnsureValid = ValidatorPos.ensurePosValid(ts, myLoc, posAgent, invertedDirection, offsetToSheep, true);
        ts.getLogger().info("--------------'DriveStrategy3::calcCenterPos' afterEnsureValid: " + afterEnsureValid.toString());                                                               //DEBUG
        return afterEnsureValid;
    }

    private Location calcTranslatedAgtPos(TransitionSystem ts, Location myLoc, SwarmManipulator swarm,
            int positionNumber, GridModel model,
            Integer offsetToSheep, Double angleIncr, RealVector directionSwarm, RealVector invertedDirection,
            ArrayList<Location> lstPosSwarm) {
        //get the sheep, which is positioned farest away from the center in a direction translated by 90° of the direction of the swarm
        double angle1 = positionNumber < 3 ? -90.0 : 90.0;
        RealVector dir1Perp = GeometryCalculator.translateAngle(ts, directionSwarm, angle1);
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' dir1Perp: " + dir1Perp.toString());                                                                       //DEBUG
        var p1 = calcPosBehindSheep(ts, swarm, 1, lstPosSwarm, dir1Perp);
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' p1: " + p1.toString());                                                                                   //DEBUG
        //create a direction, that is angular translated by 90° to the direction of interest
        RealVector dir1 = directionSwarm;
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' dir1: " + dir1.toString());                                                                               //DEBUG

        double angle = DriveStrategyCommon.calcAngle(ts, positionNumber, angleIncr);
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' angle: " + angle);                                                                                        //DEBUG
        //get the sheep, which is positioned farest away from the center in a direction translated by angle° of the inverted direction of the swarm
        RealVector dir2Perp = GeometryCalculator.translateAngle(ts, invertedDirection, angle);
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' dir2Perp: " + dir2Perp.toString());                                                                       //DEBUG
        var p2 = calcPosBehindSheep(ts, swarm, 1, lstPosSwarm, dir2Perp);
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' p2: " + p2.toString());                                                                                   //DEBUG
        //create a direction, that is angular translated by 90° to the direction of interest
        RealVector dir2 = GeometryCalculator.translateAngle(ts, dir2Perp, 90.0);
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' dir2: " + dir2.toString());                                                                               //DEBUG

        Location p5;
        try {
            Location p3 = GeometryCalculator.calcStraightIntersection(ts, p1, dir1, p2, dir2);
            //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' p3: " + p3.toString());                                                                   //DEBUG
            if (GeometryCalculator.calcDistanceInDir(swarm.getCenter(), p3, directionSwarm) >= 0) {
                //p3 is at height of center or above
                p3 = GeometryCalculator.translateInDir(ts, p2, dir1Perp,
                        GeometryCalculator.calcDistanceInDir(p2, p1, dir1Perp));
            }
            // add offset , since it was not given before the intersection
            //Location p4 = GeometryCalculator.translateInDir(ts, p3, dir1Perp, 1);
            p5 = GeometryCalculator.translateInDir(ts, p3, dir2Perp, offsetToSheep);
        } catch (InvalidAlgorithmParameterException e) {
            //should not happen since directions cant be parallel to each other
            throw new RuntimeException(e);
        }
        Location afterEnsureValid = ValidatorPos.ensurePosValid(ts, myLoc, p5, dir2Perp, offsetToSheep, true);
        //ts.getLogger().info("--------------'DriveStrategy3::calcTranslatedAgtPos' afterEnsureValid: " + afterEnsureValid.toString());                                                       //DEBUG
        return afterEnsureValid;
    }

    private Location calcPosBehindSheep(TransitionSystem ts, SwarmManipulator swarm, Integer offset,
            ArrayList<Location> lstPosSwarm, RealVector directionFromCenter) {
        //ts.getLogger().info("--------------'DriveStrategy3::calcPosBehindSheep' directionFromCenter: " + directionFromCenter.toString());                                                                //DEBUG
        //get the sheep, which is positioned farest away from the center in the specified direction
        CompDistanceInDir comp2 = new CompDistanceInDir(ts, directionFromCenter, swarm.getCenter());
        Location sheep;
        try{
            sheep = Collections.max(lstPosSwarm, comp2);
        }catch(NoSuchElementException e){
            sheep = swarm.getCenter();
        }
        //ts.getLogger().info("--------------'DriveStrategy3::calcPosBehindSheep' sheep: " + sheep.toString());                                                                             //DEBUG
        //create a location in the desired direction but behind the sheep with an offset
        double distanceSheep = Math
                .abs(GeometryCalculator.calcDistanceInDir(swarm.getCenter(), sheep, directionFromCenter));
        double distanceToCenter = distanceSheep + offset;
        var p2 = GeometryCalculator.translateInDir(ts, swarm.getCenter(), directionFromCenter, distanceToCenter);
        //ts.getLogger().info("--------------'DriveStrategy3::calcPosBehindSheep' p2: " + p2.toString());                                                                                     //DEBUG
        return p2;
    }

    class CompDistanceInDir implements Comparator<Location> {

        private RealVector direction;
        private Location origin;
        private TransitionSystem ts;

        public CompDistanceInDir(TransitionSystem ts, RealVector direction, Location origin) {
            this.direction = direction;
            this.origin = origin;
            this.ts = ts;
        }

        @Override
        public int compare(Location o1, Location o2) {
            //ts.getLogger().info("--------------'CompDistanceInDir::compare' o1: " + o1.toString());                                                                                     //DEBUG
            //ts.getLogger().info("--------------'CompDistanceInDir::compare' o2: " + o2.toString());                                                                                     //DEBUG
            double dist1 = GeometryCalculator.calcDistanceInDir(origin, o1, direction);
            double dist2 = GeometryCalculator.calcDistanceInDir(origin, o2, direction);
            //ts.getLogger().info("--------------'CompDistanceInDir::compare' dist1: " + dist1);                                                                                     //DEBUG
            //ts.getLogger().info("--------------'CompDistanceInDir::compare' dist2: " + dist2);                                                                                     //DEBUG

            if (dist1 == dist2)
                return 0;
            int result = dist1 > dist2 ? 1 : -1;
            //ts.getLogger().info("--------------'CompDistanceInDir::compare' result: " + result);                                                                                     //DEBUG
            return result;
        }

    }
}
