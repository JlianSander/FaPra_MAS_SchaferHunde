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

    protected DStarLite ds;
    private GridProcessor gridProcessor;
    private int user;
    private Set<Location> customExcludedObjects = new HashSet<>();
    protected static final ConcurrentHashMap<Pathfinder, AtomicBoolean> instances = new ConcurrentHashMap<>();

    protected Pathfinder() {
        ds = new DStarLite();
        GridModel model = GridModel.getInstance();
        gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
    }

    public static Pathfinder getInstance(Integer user) {
        for (var entry : instances.entrySet()) {
            if (entry.getValue().compareAndSet(false, true)) {
                Pathfinder instance = entry.getKey();
                instance.user = user;
                return instance;
            }
        }

        synchronized (Pathfinder.class) {
            Pathfinder pf = new Pathfinder();
            instances.put(pf, new AtomicBoolean(true));
            pf.user = user;
            return pf;
        }
    }

    protected void releaseInstance() {
        customExcludedObjects.clear();
        AtomicBoolean inUse = instances.get(this);
        if (inUse != null) {
            inUse.set(false);
        }
    }

    private void excludeObstacles() {
        GridModel model = GridModel.getInstance();
        ObstacleMap obstacleMap = model.getObstacleMap();
        gridProcessor.processEntireGrid(loc -> obstacleMap.isObstacle(loc.x, loc.y, user),
                loc -> ds.updateCell(loc.x, loc.y, -1),
                c -> false);

        // make a virtual wall around the grid
        for (int x = -1; x <= model.getWidth(); x++) {
            for (int y = -1; y <= model.getHeight(); y++) {
                if (x < 0 || x >= model.getWidth() || y < 0 || y >= model.getHeight()) {
                    ds.updateCell(x, y, -1);
                }
            }
        }

        excludeCustomObjects();
    }

    private void excludeCustomObjects() {
        for (Location location : customExcludedObjects) {
            //System.out.println("Excluding custom object at: " + location);
            ds.updateCell(location.x, location.y, -1);
        }
    }

    public void excludeObjects(Location callerPosition, int objectType, int range) {
        GridModel model = GridModel.getInstance();
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
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

    protected boolean targetIsWalkable(Location target) {
        return !(GridModel.getInstance().getObstacleMap().isObstacle(target, user)
                || customExcludedObjects.contains(target));
    }

    private List<Location> getPath(Location start, Location target)
            throws UnwalkableTargetCellException, NoPathFoundException {
        if (start.equals(target)) {
            return List.of(start);
        }

        if (!targetIsWalkable(target)) {
            throw new UnwalkableTargetCellException("Target location is an obstacle");
        }

        GridModel model = GridModel.getInstance();
        ds.init(start.x, start.y, target.x, target.y);
        excludeObstacles();
        if (!ds.replan() || ds.getPath().stream().anyMatch(s -> s.x < 0 || s.y < 0
                || s.x >= model.getWidth() || s.y >= model.getHeight())) {
            throw new NoPathFoundException("No path found");
        }
        return ds.getPath().stream().map(s -> new Location(s.x, s.y)).collect(Collectors.toList());
    }
}