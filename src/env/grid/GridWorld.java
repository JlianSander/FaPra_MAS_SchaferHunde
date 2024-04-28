package grid;

import cartago.*;
import grid.util.GridProcessor;
import grid.util.Pathfinder;
import jason.environment.grid.Location;

public class GridWorld extends Artifact {
    GridModel model;
    GridView view;
    Pathfinder pathfinder;

    void init(int size, int corralWidth, int corralHeight) {
        model = GridModel.create(size, corralWidth, corralHeight);
        commonInit(model);
    }

    void init(String filePath) {
        model = GridModel.create(filePath);
        commonInit(model);
    }

    void commonInit(GridModel model) {
        view = new GridView(model);
        defineObsProperty("gridSize", model.getWidth());
        pathfinder = new Pathfinder(model);
    }

    @OPERATION
    void dstar(OpFeedbackParam<Integer> newX, OpFeedbackParam<Integer> newY) {
        int agentId = this.getCurrentOpAgentId().getLocalId();
        // int newX = Math.random() < 0.5 ? 1 : -1;
        // int newY = Math.random() < 0.5 ? 1 : -1;
        // move(agentId, newX, newY);

        Location startPos = model.getAgPos(agentId);
        // Location targetPos = model.getFreePos();
        Location targetPos = new Location(model.getWidth() - 1, model.getHeight() - 1);
        try {
            Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
            moveTo(agentId, nextPos, newX, newY);
        } catch (Exception e) {
            failed("no next step possible");
        }
    }

    /**
     * This method moves the current agent to the next cell on his way to the specified destination defined by the specified X and Y value.
     * @param targetX Value of the final destination on the X-axis.
     * @param targetY Value of the final destination on the Y-axis.
     * @param newX Out-Parameter to inform the agent of his new position on the X-axis.
     * @param newY Out-Parameter to inform the agent of his new position on the Y-axis.
     */
    @OPERATION
    void nextStep(int targetX, int targetY, OpFeedbackParam<Integer> newX, OpFeedbackParam<Integer> newY) {
        int agentId = this.getCurrentOpAgentId().getLocalId();
        Location startPos = model.getAgPos(agentId);
        Location targetPos = new Location(targetX, targetY);
        try {
            Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
            moveTo(agentId, nextPos, newX, newY);
        } catch (Exception e) {
            failed("no next step possible");
        }
    }

    private void moveTo(int agentId, Location location, OpFeedbackParam<Integer> newX, OpFeedbackParam<Integer> newY) {
        try {
            if (model.isFree(location.x, location.y)) {
                model.setAgPos(agentId, location.x, location.y);
                newX.set(location.x);
                newY.set(location.y);
            } else {
                failed("move_failed");
            }
        } catch (Exception e) {
            failed("move_failed");
        }
    }

    @OPERATION
    void place_sheep(OpFeedbackParam<Integer> X, OpFeedbackParam<Integer> Y) {
        placeAgent(X, Y);
    }

    @OPERATION
    void place_hound(OpFeedbackParam<Integer> X, OpFeedbackParam<Integer> Y) {
        placeAgent(X, Y);
    }

    private void placeAgent(OpFeedbackParam<Integer> X, OpFeedbackParam<Integer> Y) {
        int agentId = this.getCurrentOpAgentId().getLocalId();

        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        gridProcessor.processEntireGrid(
                loc -> model.isFree(loc),
                loc -> {
                    model.setAgPos(agentId, loc);
                    X.set(loc.x);
                    Y.set(loc.y);
                },
                c -> c == 1);
    }

    // Leaving this here for a few commits just in case we do end up needing it.
    // /**
    //  * This method return the identifier, that is used to identify the agent
    //  * in the grid.
    //  * @param ID Identifier of the current agent in the grid artifact.
    //  */
    // @OPERATION
    // public void getOwnID(OpFeedbackParam<Integer> ID) {
    //     ID.set(this.getCurrentOpAgentId().getLocalId());
    // }
}