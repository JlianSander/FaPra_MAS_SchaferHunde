package jia.util;

import org.apache.commons.math3.linear.*;

import jason.environment.grid.Location;

import grid.GridModel;
import grid.util.Pathfinder;

public class SwarmManipulator {

    private Location center;
    private int radius;

    public SwarmManipulator(Location center, int radius) {
        this.center = center;
        this.radius = radius;
    }

    public Location getNextPositionTo(Location targetLocation) {
        var nextPosCenter = Pathfinder.getInstance(GridModel.SHEEP).getNextPosition(this.center, targetLocation);

        // Vector direction = new Vector(
        //     nextPosCenter.x - this.center.x,
        //     nextPosCenter.y - this.center.y
        // );
        RealVector direction = MatrixUtils.createRealVector(new double[] {
                nextPosCenter.x - this.center.x,
                nextPosCenter.y - this.center.y
        });

        var edgePosTR = new Location(nextPosCenter.x + radius, nextPosCenter.y + radius);
        var edgePosTL = new Location(nextPosCenter.x + radius, nextPosCenter.y - radius);
        var edgePosBL = new Location(nextPosCenter.x - radius, nextPosCenter.y - radius);
        var edgePosBR = new Location(nextPosCenter.x - radius, nextPosCenter.y + radius);

        boolean trBlocked = GridModel.getInstance().getObjectsAt(edgePosTR.x, edgePosTR.y).contains(GridModel.OBSTACLE);
        boolean tlBlocked = GridModel.getInstance().getObjectsAt(edgePosTL.x, edgePosTL.y).contains(GridModel.OBSTACLE);
        boolean blBlocked = GridModel.getInstance().getObjectsAt(edgePosBL.x, edgePosBL.y).contains(GridModel.OBSTACLE);
        boolean brBlocked = GridModel.getInstance().getObjectsAt(edgePosBR.x, edgePosBR.y).contains(GridModel.OBSTACLE);

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

    public Location center() {
        return center;
    }

    public int radius() {
        return radius;
    }
}
