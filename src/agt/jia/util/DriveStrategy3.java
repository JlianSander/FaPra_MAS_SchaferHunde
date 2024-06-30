package jia.util;

import org.apache.commons.math3.linear.MatrixUtils;
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

        var posSheep = BeliefBaseManager.getPosOfSheep(ts, swarm);
        
        //TODO get sheep most left
        //TODO get sheep most in direction of angular translation
        RealVector vertical = MatrixUtils.createRealVector(new double[] {0,1});
        //getIntersection(sPosLeft, vertical, sPosDir, Dir);

        return posSheep[positionNumber-1];
    }

    private Location getIntersection(Location p1, RealVector dir1, Location p2, RealVector dir2){

    }
}
