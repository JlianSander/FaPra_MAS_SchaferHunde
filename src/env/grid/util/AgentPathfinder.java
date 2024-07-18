package grid.util;

import java.util.ArrayList;
import java.util.List;

import grid.GridModel;
import jason.environment.grid.Location;

/**
 * Backwards-compatibility (behaves like old sheep pathfinder)
 */
public class AgentPathfinder extends Pathfinder {
    protected AgentPathfinder() {
        this(40000);
    }

    protected AgentPathfinder(int maxSteps) {
        super(maxSteps);
    }

    public static AgentPathfinder getInstance() {
        return Pathfinder.getInstance(AgentPathfinder.class);
    }

    @Override
    protected void excludeObstacles() {
        GridModel model = GridModel.getInstance();
        ObstacleMap obstacleMap = model.getObstacleMap();
        gridProcessor.processEntireGrid(loc -> obstacleMap.isBlocked(loc.x, loc.y, GridModel.SHEEP),
                loc -> ds.updateCell(loc.x, loc.y, -1),
                c -> false);

        excludeCustomObjects();
    }

    protected void excludeCustomObjects() {
        for (Location location : customExcludedObjects) {
            //System.out.println("Excluding custom object at: " + location);
            ds.updateCell(location.x, location.y, -1);
        }
    }

    public void excludeCustomObjects(Location callerPosition, int objectType, int range) {
        GridModel model = GridModel.getInstance();
        List<Location> objectLocations = new ArrayList<>();
        gridProcessor.processEntireGrid(
                loc -> !loc.equals(callerPosition)
                        && GridModel.getInstance().getObjectsAt(loc.x, loc.y).contains(objectType),
                loc -> objectLocations.add(loc),
                c -> false);

        for (Location location : objectLocations) {
            customExcludedObjects.add(location);
            customExcludedObjects.addAll(model.getNeighborhood(location, range, loc -> true));
        }

        customExcludedObjects.remove(callerPosition);
    }

    @Override
    protected boolean targetIsWalkable(Location target) {
        return !(GridModel.getInstance().getObstacleMap().isBlocked(target, GridModel.SHEEP)
                || customExcludedObjects.contains(target));
    }
}