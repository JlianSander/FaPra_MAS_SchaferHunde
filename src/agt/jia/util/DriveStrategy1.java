package jia.util;
import org.apache.commons.math3.linear.*;

import jason.environment.grid.Location;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;

import grid.GridModel;
import util.PropertiesLoader;

public class DriveStrategy1 implements IDrivePositioner {

    public Location calculateAgentPosition(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber) throws ExceptionPositioningFailed{
        //ts.getLogger().info("--------------'DriveStrategy1::calculateAgentPosition' positionNumber: " + positionNumber);                                                            //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer houndDistanceToSwarm = loader.getProperty("hound_keep_distance_to_sheep", Integer.class);
        Double angleIncr = loader.getProperty("hound_drive_angle_increment", Double.class);
        //ts.getLogger().info("--------------'DriveStrategy1::calculateAgentPosition' angleIncr: " + angleIncr);                                                                      //DEBUG

        RealVector directionSwarm = swarm.getDirectionTo(ts, corral.center());
        //ts.getLogger().info("--------------'DriveStrategy1::calculateAgentPosition' directionSwarm: " + directionSwarm);                                                            //DEBUG
        RealVector invertedDirection = directionSwarm.mapMultiply(-1.0);
        //ts.getLogger().info("--------------'DriveStrategy1::calculateAgentPosition' invertedDirection: " + invertedDirection);                                                      //DEBUG
        
        return DriveStrategyCommon.calculatePosition(ts, positionNumber, angleIncr, invertedDirection, swarm.getRadius(), houndDistanceToSwarm, swarm.getCenter(), model);
    }
}
