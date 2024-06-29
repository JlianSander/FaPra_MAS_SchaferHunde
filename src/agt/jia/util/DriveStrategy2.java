package jia.util;
import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;
import jason.environment.grid.Location;
import util.PropertiesLoader;

public class DriveStrategy2 implements IDrivePositioner {

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
       
        //calculate radius for quadrant associated with the position
        int radiusInQuadrant = 0;
        switch (positionNumber) {
            case 1:
                radiusInQuadrant = swarm.getRadiusQ()[1] > swarm.getRadiusQ()[2] ? swarm.getRadiusQ()[1] : swarm.getRadiusQ()[2];
                break;
            case 2:
                radiusInQuadrant = swarm.getRadiusQ()[1] > swarm.getRadiusQ()[2] ? swarm.getRadiusQ()[1] : swarm.getRadiusQ()[2];
                break;
            case 3:
                radiusInQuadrant = swarm.getRadiusQ()[2] > swarm.getRadiusQ()[3] ? swarm.getRadiusQ()[2] : swarm.getRadiusQ()[3];
                break;
            case 4:
                radiusInQuadrant = swarm.getRadiusQ()[0] > swarm.getRadiusQ()[3] ? swarm.getRadiusQ()[0] : swarm.getRadiusQ()[3];
                break;
            case 5:
                radiusInQuadrant = swarm.getRadiusQ()[0] > swarm.getRadiusQ()[3] ? swarm.getRadiusQ()[0] : swarm.getRadiusQ()[3];
                break;
            default:
                break;
        }


       return DriveStrategyCommon.calculatePosition(ts, positionNumber, angleIncr, invertedDirection, radiusInQuadrant, houndDistanceToSwarm, swarm.getCenter(), model);
    }
    
}
