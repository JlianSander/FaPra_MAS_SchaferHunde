package grid.util;

import java.util.List;

import grid.GridModel;
import jason.environment.grid.Location;

public class ObstacleMap {
    private boolean[][] data;

    public ObstacleMap(int width, int height) {
        this.data = new boolean[width][height];
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

    public void addObstacle(GridModel model, int x, int y) {
        data[x][y] = true;
    }

    public void agentMoved(GridModel model, Location oldLoc, Location newLoc) {
        if (oldLoc != null) {
            data[oldLoc.x][oldLoc.y] = locIsBlocked(model.getObjectsAt(oldLoc.x, oldLoc.y), -1);
        }
        data[newLoc.x][newLoc.y] = locIsBlocked(model.getObjectsAt(newLoc.x, newLoc.y), -1);
    }

    private boolean locIsBlocked(List<Integer> objects, int user) {
        boolean isDefaultBlocked = objects.stream()
                .anyMatch(obj -> obj == GridModel.HOUND || obj == GridModel.SHEEP || obj == GridModel.OBSTACLE);

        if (user == GridModel.HOUND) {
            return isDefaultBlocked || objects.stream().anyMatch(obj -> obj == GridModel.CORRAL);
        }

        return isDefaultBlocked;
    }
}