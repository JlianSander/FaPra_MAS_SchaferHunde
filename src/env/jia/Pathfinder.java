package jia;

import grid.GridModel;
import grid.util.GridProcessor;
import jason.environment.grid.Location;

public class Pathfinder {
    DStarLite ds;
    GridModel model;

    public Pathfinder(GridModel model) {
        ds = new DStarLite();
        this.model = model;
        excludeObstacles(model);
    }

    void excludeObstacles(GridModel model) {
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        gridProcessor.processEntireGrid(loc -> model.hasObject(GridModel.OBSTACLE, loc),
                loc -> ds.updateCell(loc.x, loc.y, -1));
    }

    public Location getNextPosition(Location start, Location target) {
        ds.updateStart(start.x, start.y);
        ds.updateGoal(target.x, target.y);
        ds.replan();
        State s = ds.getPath().get(1);
        return new Location(s.x, s.y);
    }
}
