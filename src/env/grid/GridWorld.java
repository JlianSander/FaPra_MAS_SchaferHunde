package grid;

import cartago.*;
import grid.util.GridProcessor;
import jason.environment.grid.Location;

import jia.Pathfinder;

public class GridWorld extends Artifact {
    GridModel model;
    GridView view;
    Pathfinder pathFinder;

    void init(int size, int corralWidth, int corralHeight) {
        model = new GridModel(size, corralWidth, corralHeight);
        commonInit(model);
    }

    void init(String filePath) {
        model = new GridModel(filePath);
        commonInit(model);
    }

    void commonInit(GridModel model) {
        view = new GridView(model);
        defineObsProperty("gridSize", model.getWidth());
        pathFinder = new Pathfinder(model);
    }

    @OPERATION
    void dstar() {
        int agentId = this.getCurrentOpAgentId().getLocalId();
        // int newX = Math.random() < 0.5 ? 1 : -1;
        // int newY = Math.random() < 0.5 ? 1 : -1;
        // move(agentId, newX, newY);

        Location startPos = model.getAgPos(agentId);
        // Location targetPos = model.getFreePos();
        Location targetPos = new Location(model.getWidth() - 1, model.getHeight() - 1);
        Location nextPos = pathFinder.getNextPosition(startPos, targetPos);
        moveTo(agentId, nextPos);
    }

    @OPERATION
    void moveTo(int agentId, Location location) {
        try {
            if (model.isFree(location.x, location.y)) {
                model.setAgPos(agentId, location.x, location.y);
                this.signal("agentMoved", agentId, location.x, location.y);
            }
        } catch (Exception e) {
            failed("move_failed");
        }
    }

    @OPERATION
    void place_sheep() {
        int agentId = this.getCurrentOpAgentId().getLocalId();

        boolean[] placed = { false };
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        gridProcessor.processEntireGrid(
                loc -> model.isFree(loc) && !placed[0],
                loc -> {
                    // model.add(GridModel.SHEEP, loc);
                    model.setAgPos(agentId, loc);
                    placed[0] = true;
                });
    }
}