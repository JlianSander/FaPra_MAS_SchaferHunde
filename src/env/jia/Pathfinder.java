package jia;

import grid.GridModel;
import grid.util.GridProcessor;
import jason.environment.grid.Location;

import java.util.List;
import java.util.stream.Collectors;

import dstarlite.DStarLite;

public class Pathfinder {
    DStarLite ds;

    public Pathfinder(GridModel model) {
        ds = new DStarLite();
        excludeObstacles(model);
    }

    private void excludeObstacles(GridModel model) {
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        gridProcessor.processEntireGrid(loc -> model.hasObject(GridModel.OBSTACLE, loc),
                loc -> ds.updateCell(loc.x, loc.y, -1),
                c -> false);
    }

    public Location getNextPosition(Location start, Location target) {
        return getPath(start, target).get(1);
    }

    /**
     * Excluding start location
     */
    public List<Location> getPath(Location start, Location target) {
        ds.updateStart(start.x, start.y);
        ds.updateGoal(target.x, target.y);
        ds.replan();
        return ds.getPath().stream().map(s -> new Location(s.x, s.y)).collect(Collectors.toList());
    }
}
