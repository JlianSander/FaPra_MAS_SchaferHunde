package grid.util;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.ConcurrentHashMap;

import grid.GridModel;
import jason.environment.grid.Location;

public class BypassPathfinder extends Pathfinder {
    private static final ConcurrentHashMap<BypassPathfinder, AtomicBoolean> instances = new ConcurrentHashMap<>();

    private BypassPathfinder() {
        super();
    }

    public static BypassPathfinder getInstance() {
        for (var entry : instances.entrySet()) {
            if (entry.getValue().compareAndSet(false, true)) {
                BypassPathfinder instance = entry.getKey();
                return instance;
            }
        }

        synchronized (BypassPathfinder.class) {
            BypassPathfinder pf = new BypassPathfinder();
            instances.put(pf, new AtomicBoolean(true));
            return pf;
        }
    }

    public static Pathfinder getInstance(Integer user) {
        throw new UnsupportedOperationException("Use regular Pathfinder instead of BypassPathfinder");
    }

    @Override
    protected void excludeObstacles() {
        makeVirtualWall();
    }

    @Override
    protected void releaseInstance() {
        AtomicBoolean inUse = instances.get(this);
        if (inUse != null) {
            inUse.set(false);
        }
    }

    @Override
    protected boolean targetIsWalkable(Location target) {
        return !GridModel.getInstance().getObjectsAt(target.x, target.y).contains(GridModel.OBSTACLE);
    }
}