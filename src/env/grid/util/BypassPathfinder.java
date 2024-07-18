package grid.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.javatuples.Pair;

import grid.GridModel;
import jason.environment.grid.Location;

public class BypassPathfinder extends Pathfinder {
    private static Map<Pair<Location, Location>, List<Location>> bypassCache;

    protected BypassPathfinder() {
        super(80000);
    }

    public static BypassPathfinder getInstance() {
        return Pathfinder.getInstance(BypassPathfinder.class);
    }

    public void prewarm() {
        if (bypassCache != null) {
            return;
        }

        bypassCache = new HashMap<>();

        gridProcessor.processEntireGrid(loc1 -> targetIsWalkable(loc1),
                loc1 -> {
                    gridProcessor.processEntireGrid(loc2 -> targetIsWalkable(loc2),
                            loc2 -> {
                                List<Location> path = getPath(loc1, loc2);
                                bypassCache.put(new Pair<>(loc1, loc2), path);
                            },
                            c -> false);
                },
                c -> false);
    }

    @Override
    protected boolean targetIsWalkable(Location target) {
        return !GridModel.getInstance().getObjectsAt(target.x, target.y).contains(GridModel.OBSTACLE);
    }

    @Override
    public Location getNextPosition(Location start, Location target)
            throws UnwalkableTargetCellException, NoPathFoundException {
        try {
            if (!targetIsWalkable(target)) {
                throw new UnwalkableTargetCellException("Target location is an obstacle");
            }

            if (!bypassCache.containsKey(new Pair<>(start, target))) {
                throw new NoPathFoundException("No path found");
            }

            List<Location> path = bypassCache.get(new Pair<>(start, target));
            return path.size() > 1 ? path.get(1) : path.get(0);
        } finally {
            releaseInstance();
        }
    }
}