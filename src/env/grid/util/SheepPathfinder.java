package grid.util;

import grid.GridModel;
import jason.environment.grid.Location;

/**
 * Sheep pathfinder with smaller max steps
 */
public class SheepPathfinder extends AgentPathfinder {
    protected SheepPathfinder() {
        super(2000);
    }

    public static SheepPathfinder getInstance() {
        return Pathfinder.getInstance(SheepPathfinder.class);
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

    @Override
    protected boolean targetIsWalkable(Location target) {
        return !(GridModel.getInstance().getObstacleMap().isBlocked(target, GridModel.SHEEP)
                || customExcludedObjects.contains(target));
    }
}