package grid.util;

import grid.GridModel;
import jason.environment.grid.Location;

public class BypassPathfinder extends Pathfinder {
    protected BypassPathfinder() {
        super(80000);
    }

    public static BypassPathfinder getInstance() {
        return Pathfinder.getInstance(BypassPathfinder.class);
    }

    @Override
    protected boolean targetIsWalkable(Location target) {
        return !GridModel.getInstance().getObjectsAt(target.x, target.y).contains(GridModel.OBSTACLE);
    }
}