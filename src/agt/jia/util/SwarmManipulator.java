package jia.util;

import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;

import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import grid.util.BypassPathfinder;

public class SwarmManipulator {

    private Location center;
    private int radius;
    private int[] radiusQ;

    public SwarmManipulator(Location center, int radius) {
        this.center = center;
        this.radius = radius;
    }

    public SwarmManipulator(Location center, int radius, int[] radiusQ) {
        this(center, radius);
        this.radiusQ = radiusQ;
    }

    public Location getNextPositionTo(TransitionSystem ts, Location targetLocation) {
        //ts.getLogger().info("--------------'getNextPositionTo' get next step from " + this.center.toString() + " to " + targetLocation.toString());                                                               //DEBUG
        var nextPosCenter = BypassPathfinder.getInstance().getNextPosition(this.center, targetLocation);
        //var nextPosCenter = Pathfinder.getInstance(GridModel.SHEEP).getNextPosition(this.center, targetLocation);
        //ts.getLogger().info("--------------'getNextPositionTo' nextPosCenter: " + nextPosCenter.toString());                                                               //DEBUG

        var edgePosTR = new Location(nextPosCenter.x + radius, nextPosCenter.y + radius);
        var edgePosTL = new Location(nextPosCenter.x + radius, nextPosCenter.y - radius);
        var edgePosBL = new Location(nextPosCenter.x - radius, nextPosCenter.y - radius);
        var edgePosBR = new Location(nextPosCenter.x - radius, nextPosCenter.y + radius);

        boolean trBlocked = GridModel.getInstance().getObjectsAt(edgePosTR.x, edgePosTR.y).contains(GridModel.OBSTACLE);
        //ts.getLogger().info("--------------'getNextPositionTo' trBlocked: " + trBlocked);                                                               //DEBUG
        boolean tlBlocked = GridModel.getInstance().getObjectsAt(edgePosTL.x, edgePosTL.y).contains(GridModel.OBSTACLE);
        //ts.getLogger().info("--------------'getNextPositionTo' tlBlocked: " + tlBlocked);                                                               //DEBUG
        boolean blBlocked = GridModel.getInstance().getObjectsAt(edgePosBL.x, edgePosBL.y).contains(GridModel.OBSTACLE);
        //ts.getLogger().info("--------------'getNextPositionTo' blBlocked: " + blBlocked);                                                               //DEBUG
        boolean brBlocked = GridModel.getInstance().getObjectsAt(edgePosBR.x, edgePosBR.y).contains(GridModel.OBSTACLE);
        //ts.getLogger().info("--------------'getNextPositionTo' brBlocked: " + brBlocked);                                                               //DEBUG

        int xPosUnBlocked = nextPosCenter.x;
        int yPosUnBlocked = nextPosCenter.y;

        if ((trBlocked || brBlocked) && !tlBlocked && !blBlocked) {
            xPosUnBlocked--;
        }
        if ((tlBlocked || blBlocked) && !trBlocked && !brBlocked) {
            xPosUnBlocked++;
        }
        if ((trBlocked || tlBlocked) && !brBlocked && !blBlocked) {
            yPosUnBlocked--;
        }
        if ((brBlocked || blBlocked) && !trBlocked && !tlBlocked) {
            yPosUnBlocked++;
        }

        return new Location(xPosUnBlocked, yPosUnBlocked);
    }

    public RealVector getDirectionTo(TransitionSystem ts, Location target){
        //get position, where the swarm is to drive to        
        //ts.getLogger().info("--------------'getDirectionTo' Swarm.Center: (" + this.center().x + "," + this.center().y + ")");                                     //DEBUG
        var nextPos = this.getNextPositionTo(ts, target);
        //ts.getLogger().info("--------------'getDirectionTo' Swarm_Next_Pos: (" + swarmTargetLoc.x + "," + swarmTargetLoc.y + ")");                                     //DEBUG

        return GeometryCalculator.calcDirection(ts, this.center, nextPos);
    }

    public Location getCenter() {
        return center;
    }

    public int getRadius() {
        return radius;
    }

    public int[] getRadiusQ(){
        return radiusQ;
    }
}
