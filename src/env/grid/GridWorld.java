package grid;

import cartago.*;
import grid.util.Pathfinder;
import jason.environment.grid.Location;
import model.AgentInfo;
import service.AgentDB;

public class GridWorld extends Artifact {
    GridModel model;
    GridView view;
    AgentDB agentDB;
    Pathfinder pathfinder;

    void init(int size, int corralWidth, int corralHeight) {
        agentDB = new AgentDB();
        model = GridModel.create(size, corralWidth, corralHeight, agentDB);
        commonInit(model);
    }

    void init(String filePath) {
        agentDB = new AgentDB();
        model = GridModel.create(filePath, agentDB);
        commonInit(model);
    }

    void commonInit(GridModel model) {
        view = new GridView(model, agentDB);
        defineObsProperty("gridSize", model.getWidth());
        pathfinder = new Pathfinder(model);
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
        Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
        moveTo(agentId, nextPos, newX, newY);
    }

    private void moveTo(int agentId, Location location, OpFeedbackParam<Integer> newX, OpFeedbackParam<Integer> newY) {
        if (model.isFree(location)) {
            Location prevPos = model.getAgPos(agentId);
            model.setAgPos(agentDB.getAgentById(agentId), location);
            newX.set(location.x);
            newY.set(location.y);
            pathfinder.agentMoved(prevPos, location);
            signal("mapChanged");
        } else {
            failed("move_failed");
        }
    }

    @OPERATION
    private void initAgent(String name, OpFeedbackParam<Integer> X, OpFeedbackParam<Integer> Y) {
        int agentId = this.getCurrentOpAgentId().getLocalId();
        AgentInfo agentInfo = agentDB.addAgent(agentId, name);
        Location loc = model.initAgent(agentInfo);
        X.set(loc.x);
        Y.set(loc.y);
        moveTo(agentId, loc, X, Y);
    }
}