package grid;

import grid.util.GridModelFileParser;
import grid.util.GridProcessor;
import jason.environment.grid.GridWorldModel;
import jason.environment.grid.Location;
import model.AgentInfo;
import service.AgentDB;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class GridModel extends GridWorldModel {
    public static final int CORRAL = 16;
    public static final int SHEEP = 32;
    public static final int HOUND = 64;

    private static GridProcessor gridProcessor;
    private static GridModel model = null;
    public AgentDB agentDB;
    private char[][] gridData;

    // Private constructor for singleton
    private GridModel(int width, int height) {
        super(width, height, 1000);
        commonInit(width, height);
    }

    public static GridModel getInstance() {
        if (model == null)
            throw new IllegalStateException("GridModel not initialized");

        return model;
    }

    // Factory method to create the singleton instance from parameters
    public static synchronized GridModel create(int size, int corralWidth, int corralHeight, AgentDB agentDB) {
        model = new GridModel(size, size);
        model.agentDB = agentDB;

        // Define corral
        int startX = 1;
        int startY = 1;
        for (int i = startX; i < startX + corralWidth; i++) {
            for (int j = startY; j < startY + corralHeight; j++) {
                model.add(CORRAL, i, j);
            }
        }

        // Initialize the grid with obstructions
        float obstacleDensity = 0.2f;
        gridProcessor.processEntireGrid(loc -> model.isFree(CORRAL, loc)
                && Math.random() < obstacleDensity,
                loc -> model.add(OBSTACLE, loc),
                c -> false);
        return getInstance();
    }

    // Factory method to create the singleton instance from file
    public static synchronized GridModel create(String filePath, AgentDB agentDB) {
        char[][] gridData = GridModelFileParser.parseGridFile(filePath);
        int width = gridData[0].length;
        int height = gridData.length;
        model = new GridModel(width, height);
        model.agentDB = agentDB;
        model.loadFromFile(gridData);
        return getInstance();
    }

    private static void commonInit(int width, int height) {
        gridProcessor = new GridProcessor(width, height);
    }

    private void loadFromFile(char[][] gridData) {
        this.gridData = gridData;
        this.width = gridData[0].length;
        this.height = gridData.length;
        this.data = new int[width][height];

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                switch (gridData[y][x]) {
                    case 'O':
                    case 'S':
                    case 'H':
                        break;
                    case 'X':
                        add(OBSTACLE, x, y);
                        break;
                    case 'C':
                        add(CORRAL, x, y);
                        break;
                    default:
                        throw new IllegalArgumentException("Invalid object type in grid template file");
                }
            }
        }
    }

    public Location initAgent(AgentInfo agentInfo) {
        if (gridData != null) {
            for (int y = 0; y < height; y++) {
                for (int x = 0; x < width; x++) {
                    if (agentInfo.getAgentType() == SHEEP && gridData[y][x] == 'S' ||
                            agentInfo.getAgentType() == HOUND && gridData[y][x] == 'H') {
                        // if (!isFree(SHEEP, x, y) || !isFree(HOUND, x, y)) {
                        if (!isFree(x, y)) {
                            continue;
                        }

                        // setAgPos(agentInfo, x, y);
                        return new Location(x, y);
                    }
                }
            }
        }

        Location location = new Location(-1, -1);
        gridProcessor.processEntireGrid(
                loc -> model.isFree(loc),
                loc -> {
                    // setAgPos(agentInfo, loc);
                    location.x = loc.x;
                    location.y = loc.y;
                },
                c -> c == 1);

        return location;
    }

    @Override
    public boolean isFree(Location l) {
        List<Integer> objects = getObjectsAt(l);
        return objects.size() == 1 && (objects.get(0) == CLEAN || objects.get(0) == CORRAL);
    }

    @Override
    public boolean isFree(int x, int y) {
        return isFree(new Location(x, y));
        // return inGrid(x, y) && (data[x][y] & SHEEP) == 0 && (data[x][y] & HOUND) == 0
        //         && (data[x][y] & 4) == 0 && (data[x][y] & 2) == 0;
    }

    @Override
    public void setAgPos(int ag, Location l) {
        AgentInfo agentInfo = agentDB.getAgentById(ag);
        setAgPos(agentInfo, l);
    }

    // setAgPos now requires AgentInfo as we need to inject the proper agent type. A wrapper is available above.
    public void setAgPos(AgentInfo agentInfo, int x, int y) {
        setAgPos(agentInfo, new Location(x, y));
    }

    public void setAgPos(AgentInfo agentInfo, Location l) {
        Location oldLoc = this.getAgPos(agentInfo.getCartagoId());
        if (oldLoc != null) {
            this.remove(agentInfo.getAgentType(), oldLoc.x, oldLoc.y);
        }

        this.agPos[agentInfo.getCartagoId()] = l;
        this.add(agentInfo.getAgentType(), l.x, l.y);
    }

    public List<Location> getNeighborhood(Location loc, int range, Predicate<Location> filter) {
        return getNeighborhood(loc, range, Integer.MAX_VALUE, filter);
    }

    public List<Location> getNeighborhood(Location loc, int range, int maxAmount, Predicate<Location> filter) {
        range = Math.clamp(range, 1, getWidth());
        List<Location> neighbors = new ArrayList<>();
        for (int dx = -range; dx <= range; dx++) {
            for (int dy = -range; dy <= range; dy++) {
                if (dx == 0 && dy == 0) {
                    continue;
                }

                int newX = loc.x + dx;
                int newY = loc.y + dy;
                Location newLoc = new Location(newX, newY);
                if (inGrid(newLoc) && filter.test(newLoc)) {
                    neighbors.add(newLoc);
                    if (neighbors.size() >= maxAmount) {
                        return neighbors;
                    }
                }
            }
        }
        return neighbors;
    }

    public Location getFirstFreeNeighbor(Location location) {
        for (int i = 1; i < getWidth(); i++) {
            List<Location> neighbors = getNeighborhood(location, i, loc -> isFree(loc));
            if (neighbors.size() > 0) {
                return neighbors.get((int) (Math.random() * neighbors.size()));
            }
        }
        return location;
    }

    /**
     * Returns a list of ALL objects for a given location. The items in this list will be sorted by significance (Hound > Sheep > Corral/Obstacle/Clean).
     * A location that is a corral and that is occupied by a sheep will return both sheep and corral, in that order.
     * @param location Location to check
     * @return List of objects at the given location
     */
    public List<Integer> getObjectsAt(Location location) {
        if (!inGrid(location)) {
            return new ArrayList<>();
        }

        int obj = data[location.x][location.y];

        List<Integer> objects = new ArrayList<>();
        switch (obj) {
            case GridModel.HOUND:
            case GridModel.HOUND + GridModel.CORRAL:
                objects.add(GridModel.HOUND);
                break;
            case GridModel.SHEEP:
            case GridModel.SHEEP + GridModel.CORRAL:
                objects.add(GridModel.SHEEP);
                break;
            case GridModel.OBSTACLE:
            case GridModel.CLEAN:
            case GridModel.CORRAL:
                objects.add(obj);
                break;
            default:
                throw new IllegalStateException("Invalid object type at: " + location);
        }

        return objects;
    }
}