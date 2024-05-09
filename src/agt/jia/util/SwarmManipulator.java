package jia.util;

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
        GridModel model = GridModel.getInstance();
        var pathFinder = new Pathfinder(model);
        var nextPosition = pathFinder.getNextPosition(this.center, targetLocation);

        //TODO  use radius to check if nextPosition is favourable/problematic for outer elements of swarm

        return nextPosition;
    }

    public Location center() {
        return center;
    }

    public int radius() {
        return radius;
    }
}
