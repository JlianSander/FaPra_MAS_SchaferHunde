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
        for (int x = 0; x < model.getWidth(); x++) {
            for (int y = 0; y < model.getHeight(); y++) {
                if (model.hasObject(GridModel.OBSTACLE, x, y)) {
                    System.out.println("excluding: " + x + ", " + y);
                    ds.updateCell(x, y, -1);
                }
            }
        }
        // GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        // gridProcessor.processEntireGrid(loc -> model.hasObject(GridModel.OBSTACLE, loc),
        //         loc -> ds.updateCell(loc.x, loc.y, -1));
    }

    public Location getNextPosition(Location start, Location target) {
        ds.init(start.x, start.y, target.x, target.y);
        excludeObstacles(model);
        ds.replan();
        State s = ds.getPath().get(1);
        System.out.println("next: " + s.x + ", " + s.y);
        return new Location(s.x, s.y);
    }
}
