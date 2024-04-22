package grid;

import cartago.*;
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

    // @OPERATION
    // void placeObstacle(int x, int y) {
    //     model.add(GridModel.OBSTACLE, x, y);
    //     view.update(x, y);
    // }
}