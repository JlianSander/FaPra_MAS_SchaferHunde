package jia.util;
import org.apache.commons.math3.linear.*;

import jason.environment.grid.Location;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;

import grid.GridModel;
import util.PropertiesLoader;

public class DriveStrategy1 implements IDrivePositioner {

    public Location calculateAgentPosition(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber) throws ExceptionPositioningFailed{
        //ts.getLogger().info("--------------'positionAgent' positionNumber: " + positionNumber);                                                                       //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer houndDistanceToSwarm = loader.getProperty("hound_keep_distance_to_swarm", Integer.class);
        Double angleIncr = loader.getProperty("hound_driving_position_angle_increment", Double.class);
        //ts.getLogger().info("--------------'positionAgent' angle_incr: " + angle_incr);                                                                               //DEBUG

        RealVector directionSwarm = swarm.getDirectionTo(ts, corral.center());
        RealVector invertedDirection = directionSwarm.mapMultiply(-1.0);
        
        return DriveStrategyCommon.calculatePosition(ts, positionNumber, angleIncr, invertedDirection, swarm.getRadius(), houndDistanceToSwarm, swarm.getCenter(), model);
    }
}
