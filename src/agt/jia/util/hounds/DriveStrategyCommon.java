package jia.util.hounds;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;
import jia.util.common.GeometryCalculator;

public class DriveStrategyCommon {

    public static Location calculatePosition(TransitionSystem ts, Location ownLocation, int positionNumber,
            Double angle_incr, RealVector directionFromCenter,
            int radiusSwarm, int offsetToSheep, Location center, GridModel model) {
        double angle = calcAngle(ts, positionNumber, angle_incr);

        Location agentPos = calcPosOnCircle(ts, center, directionFromCenter, angle, radiusSwarm + offsetToSheep + 1);

        return ValidatorPos.ensurePosValid(ts, ownLocation, agentPos, directionFromCenter, offsetToSheep, true);
    }

    public static double calcAngle(TransitionSystem ts, int positionNumber, Double angle_incr) {
        //calculate angle depending on the positionNumber
        double angle = (3 - positionNumber) * angle_incr;
        //ts.getLogger().info("--------------'DriveStrategyCommon::calcAngle' angle: " + angle);                                                                             //DEBUG
        return angle;
    }

    public static Location calcPosOnCircle(TransitionSystem ts, Location center, RealVector directionFromCenter,
            double angleInDegree, int radius) {
        RealVector directionSwarmRotated = GeometryCalculator.translateAngle(ts, directionFromCenter, angleInDegree);
        //ts.getLogger().info("--------------'DriveStrategyCommon::positionAgent' directionSwarmRotated: " + directionSwarmRotated.toString());                 //DEBUG

        //calculate agents position behind the swarm
        var agentPos = GeometryCalculator.translateInDir(ts, center, directionSwarmRotated, radius);
        //ts.getLogger().info("--------------'DriveStrategyCommon::positionAgent' agentPos: " + agentPos.toString());                 //DEBUG
        return agentPos;
    }
}
