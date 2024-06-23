package grid.util;

import java.util.List;

import grid.GridModel;
import jason.environment.grid.Location;
import util.PropertiesLoader;

public class ObstacleMap {
    private boolean[][] data;
    private boolean houndWalkThroughCorral;

    public ObstacleMap(int width, int height) {
        this.data = new boolean[width][height];
        PropertiesLoader loader = PropertiesLoader.getInstance();
        houndWalkThroughCorral = loader.getProperty("hound_walk_through_corral", Boolean.class);
    }

    public boolean isObstacle(int x, int y, int user) {
        if (x < 0 || y < 0 || x >= data.length || y >= data[0].length) {
            return true;
        }
        return data[x][y] || locIsBlocked(GridModel.getInstance().getObjectsAt(x, y), user);
    }

    public boolean isObstacle(Location loc, int user) {
        return isObstacle(loc.x, loc.y, user);
    }

    public void addObstacle(int x, int y) {
        data[x][y] = true;
    }

    public void agentMoved(Location oldLoc, Location newLoc) {
        if (newLoc == null) {
            throw new IllegalArgumentException("New location cannot be null");
        }
        GridModel model = GridModel.getInstance();
        if (oldLoc != null) {
            data[oldLoc.x][oldLoc.y] = locIsBlocked(model.getObjectsAt(oldLoc.x, oldLoc.y), -1);
        }
        data[newLoc.x][newLoc.y] = locIsBlocked(model.getObjectsAt(newLoc.x, newLoc.y), -1);
    }

    public void removeAgent(Location loc) {
        data[loc.x][loc.y] = false;
    }

    private boolean locIsBlocked(List<Integer> objects, int user) {
        boolean isDefaultBlocked = objects.stream()
                .anyMatch(obj -> obj == GridModel.HOUND || obj == GridModel.SHEEP || obj == GridModel.OBSTACLE);

        if (!houndWalkThroughCorral && user == GridModel.HOUND) {
            return isDefaultBlocked || objects.stream().anyMatch(obj -> obj == GridModel.CORRAL);
        }

        return isDefaultBlocked;
    }
}