package grid.util;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import dstarlite.DStarLite;
import jason.environment.grid.Location;
import grid.GridModel;

public class Pathfinder {
    public class UnwalkableTargetCellException extends RuntimeException {
        public UnwalkableTargetCellException(String message) {
            super(message);
        }
    }

    private DStarLite ds;
    private GridProcessor gridProcessor;
    private int user;
    private Set<Location> customExcludedObjects = new HashSet<>();
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

    private void releaseInstance() {
        customExcludedObjects.clear();
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
        excludeCustomObjects();
    }

    private void excludeCustomObjects() {
        for (Location location : customExcludedObjects) {
            System.out.println("Excluding custom object at: " + location);
            ds.updateCell(location.x, location.y, -1);
        }
    }

    public void excludeObjects(Location callerPosition, int objectType, int range) {
        GridProcessor gridProcessor = new GridProcessor(GridModel.getInstance().getWidth(),
                GridModel.getInstance().getHeight());
        List<Location> objectLocations = new ArrayList<>();
        gridProcessor.processEntireGrid(
                loc -> !loc.equals(callerPosition)
                        && GridModel.getInstance().getObjectsAt(loc.x, loc.y).contains(objectType),
                loc -> objectLocations.add(loc),
                c -> false);

        for (Location location : objectLocations) {
            customExcludedObjects.add(location);
            customExcludedObjects.addAll(GridModel.getInstance().getNeighborhood(location, range, loc -> true));
        }

        customExcludedObjects.remove(callerPosition);
    }

    public Location getNextPosition(Location start, Location target) {
        List<Location> path = getPath(start, target);
        releaseInstance();
        return path.size() > 1 ? path.get(1) : path.get(0);
    }

    private List<Location> getPath(Location start, Location target) throws UnwalkableTargetCellException {
        if (start.equals(target)) {
            return List.of(start);
        }

        if (GridModel.getInstance().getObstacleMap().isObstacle(target, user)
                || customExcludedObjects.contains(target)) {
            throw new UnwalkableTargetCellException("Target location is an obstacle");
        }

        ds.init(start.x, start.y, target.x, target.y);
        excludeObstacles();
        ds.replan();
        return ds.getPath().stream().map(s -> new Location(s.x, s.y)).collect(Collectors.toList());
    }
}