package jia.util;

import java.util.Iterator;
import java.util.List;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.asSyntax.Literal;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.asSyntax.ListTermImpl;
import jason.asSyntax.ListTerm;

import jason.environment.grid.Area;
import jason.environment.grid.Location;
import jason.stdlib.foreach;
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

        var posSheep = BeliefBaseManager.getPosOfSheep(ts, swarm);
        
        return posSheep[positionNumber-1];
    }
}
