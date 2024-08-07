package grid.util;

import grid.GridModel;
import jason.environment.grid.Location;

/**
 * Behaves like old hound pathfinder
 */
public class HoundPathfinder extends AgentPathfinder {
    public static HoundPathfinder getInstance() {
        return Pathfinder.getInstance(HoundPathfinder.class);
    }

    @Override
    protected void excludeObstacles() {
        GridModel model = GridModel.getInstance();
        ObstacleMap obstacleMap = model.getObstacleMap();
        gridProcessor.processEntireGrid(loc -> obstacleMap.isBlocked(loc.x, loc.y, GridModel.HOUND),
                loc -> ds.updateCell(loc.x, loc.y, -1),
                c -> false);

        excludeCustomObjects();
    }

    @Override
    protected boolean targetIsWalkable(Location target) {
        return !(GridModel.getInstance().getObstacleMap().isBlocked(target, GridModel.HOUND)
                || customExcludedObjects.contains(target));
    }
}