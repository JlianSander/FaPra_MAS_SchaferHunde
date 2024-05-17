package grid.util;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.List;
import java.util.stream.Collectors;

import dstarlite.DStarLite;

import jason.environment.grid.Location;

import grid.GridModel;

public class Pathfinder {
    private DStarLite ds;
    private GridProcessor gridProcessor;
    private int user;
    private static final ConcurrentHashMap<Pathfinder, AtomicBoolean> instances = new ConcurrentHashMap<>();

    private Pathfinder() {
        ds = new DStarLite();
        gridProcessor = new GridProcessor(GridModel.getInstance().getWidth(), GridModel.getInstance().getHeight());
    }

    public synchronized static Pathfinder getInstance(Integer user) {
        for (var entry : instances.entrySet()) {
            if (entry.getValue().compareAndSet(false, true)) {
                entry.getKey().user = user;
                return entry.getKey();
            }
        }

        Pathfinder pf = new Pathfinder();
        instances.put(pf, new AtomicBoolean(true));
        pf.user = user;
        return pf;
    }

    public void releaseInstance() {
        AtomicBoolean inUse = instances.get(this);
        if (inUse != null) {
            inUse.set(false);
        }
    }

    private void excludeObstacles() {
        ObstacleMap obstacleMap = GridModel.getInstance().getObstacleMap();
        gridProcessor.processEntireGrid(loc -> obstacleMap.isObstacle(loc.x, loc.y, user),
                loc -> ds.updateCell(loc.x, loc.y, -1),
                c -> false);
    }

    public Location getNextPosition(Location start, Location target) {
        List<Location> path = getPath(start, target);
        return path.size() > 1 ? path.get(1) : path.get(0);
    }

    private List<Location> getPath(Location start, Location target) {
        ds.init(start.x, start.y, target.x, target.y);
        excludeObstacles();
        ds.replan();
        return ds.getPath().stream().map(s -> new Location(s.x, s.y)).collect(Collectors.toList());
    }
}