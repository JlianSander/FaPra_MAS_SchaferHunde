package grid.util;

import java.util.logging.Logger;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import jason.environment.grid.Location;

import dstarlite.DStarLite;
import grid.GridModel;

public class Pathfinder {
    public class UnwalkableTargetCellException extends RuntimeException {
        public UnwalkableTargetCellException(String message) {
            super(message);
        }
    }

    public class NoPathFoundException extends RuntimeException {
        public NoPathFoundException(String message) {
            super(message);
        }
    }

    private static final Logger logger = Logger.getLogger(Pathfinder.class.getName());

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

    public Location getNextPosition(Location start, Location target)
            throws UnwalkableTargetCellException, NoPathFoundException {
        try {
            List<Location> path = getPath(start, target);
            return path.size() > 1 ? path.get(1) : path.get(0);
        } catch (UnwalkableTargetCellException | NoPathFoundException e) {
            logger.info(e.getMessage() + " -> Returning start location instead");
            return start;
        } finally {
            releaseInstance();
        }
    }

    private List<Location> getPath(Location start, Location target)
            throws UnwalkableTargetCellException, NoPathFoundException {
        if (start.equals(target)) {
            return List.of(start);
        }

        if (GridModel.getInstance().getObstacleMap().isObstacle(target, user)
                || customExcludedObjects.contains(target)) {
            throw new UnwalkableTargetCellException("Target location is an obstacle");
        }

        ds.init(start.x, start.y, target.x, target.y);
        excludeObstacles();
        if (!ds.replan() || ds.getPath().stream().anyMatch(s -> s.x < 0 || s.y < 0
                || s.x >= GridModel.getInstance().getWidth() || s.y >= GridModel.getInstance().getHeight())) {
            throw new NoPathFoundException("No path found");
        }
        return ds.getPath().stream().map(s -> new Location(s.x, s.y)).collect(Collectors.toList());
    }
}