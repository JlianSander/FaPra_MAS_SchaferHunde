package grid;

import cartago.*;
import grid.util.GridProcessor;
import jason.environment.grid.Location;

public class GridWorld extends Artifact {
    GridModel model;
    GridView view;

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
    }

    @OPERATION
    void move(int agentId, int dx, int dy) {
        try {
            Location loc = model.getAgPos(agentId);
            int x = loc.x + dx;
            int y = loc.y + dy;
            if (model.isFreeOfObstacle(x, y)) {
                model.setAgPos(agentId, x, y);
                this.signal("agentMoved", agentId, x, y);
            }
        } catch (Exception e) {
            failed("move_failed");
        }
    }

    @OPERATION
    void place_sheep() {
        // int agentId = this.getCurrentOpAgentId().getLocalId();

        boolean[] placed = { false };
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        gridProcessor.processEntireGrid(
                loc -> model.isFree(GridModel.SHEEP, loc) && !placed[0],
                loc -> {
                    model.add(GridModel.SHEEP, loc);
                    placed[0] = true;
                });

        view.repaint();
        view.update();
    }
}