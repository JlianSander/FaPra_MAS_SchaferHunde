package jia.util;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;
import jason.environment.grid.Location;
import util.PropertiesLoader;

public class DriveStrategy2 implements IDrivePositioner {

    @Override
    public Location calculateAgentPosition(TransitionSystem ts, Location myLoc, SwarmManipulator swarm, Area corral, int positionNumber){
        //ts.getLogger().info("--------------'positionAgent' positionNumber: " + positionNumber);                                                                       //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer houndDistanceToSwarm = loader.getProperty("hound_keep_distance_to_sheep", Integer.class);
        Double angleIncr = loader.getProperty("hound_drive_angle_increment", Double.class);
        //ts.getLogger().info("--------------'positionAgent' angle_incr: " + angle_incr);                                                                               //DEBUG

        RealVector directionSwarm = swarm.getDirectionTo(ts, corral.center());
        RealVector invertedDirection = directionSwarm.mapMultiply(-1.0);

        var lstPosSwarm = BeliefBaseManager.getPosOfSheep(ts, swarm);
        //ts.getLogger().info("--------------'DriveStrategy2::calculateAgentPosition' lstPosSwarm: " + lstPosSwarm.toString());                                        //DEBUG
        var radiusQ = calculateQuadrantRadius(lstPosSwarm, swarm);
        //calculate radius for quadrant associated with the position
        int radiusInQuadrant = 0;
        switch (positionNumber) {
            case 1:
                radiusInQuadrant = radiusQ[1] > radiusQ[2] ? radiusQ[1] : radiusQ[2];
                break;
            case 2:
                radiusInQuadrant = radiusQ[1] > radiusQ[2] ? radiusQ[1] : radiusQ[2];
                break;
            case 3:
                radiusInQuadrant = radiusQ[2] > radiusQ[3] ? radiusQ[2] : radiusQ[3];
                break;
            case 4:
                radiusInQuadrant = radiusQ[0] > radiusQ[3] ? radiusQ[0] : radiusQ[3];
                break;
            case 5:
                radiusInQuadrant = radiusQ[0] > radiusQ[3] ? radiusQ[0] : radiusQ[3];
                break;
            default:
                break;
        }

       return DriveStrategyCommon.calculatePosition(ts, myLoc, positionNumber, angleIncr, invertedDirection, radiusInQuadrant, houndDistanceToSwarm, swarm.getCenter(), model);
    }
    

    private int[] calculateQuadrantRadius(ArrayList<Location> posSheep, SwarmManipulator swarm){
        int[] radiusQ = new int[4];
        var center = swarm.getCenter();
        radiusQ[0] = getRadiusInQ(posSheep, center, loc -> loc.x >= center.x && loc.y <= center.y);
        radiusQ[1] = getRadiusInQ(posSheep, center, loc -> loc.x <= center.x && loc.y <= center.y);
        radiusQ[2] = getRadiusInQ(posSheep, center, loc -> loc.x <= center.x && loc.y >= center.y);
        radiusQ[3] = getRadiusInQ(posSheep, center, loc -> loc.x >= center.x && loc.y >= center.y);
        return radiusQ;
    }


    private int getRadiusInQ(ArrayList<Location> posSheep, Location center, Predicate<Location> qCondition) {
        List<Location> sheepInQuadrant = posSheep.stream().filter(qCondition).collect(Collectors.toList());
        var listRadius = sheepInQuadrant.stream().map( loc ->loc.distanceChebyshev(center)).collect(Collectors.toList());
        return Collections.max(listRadius);
    }
}
