package grid;

import java.util.logging.Logger;

import cartago.*;
import jason.environment.grid.Location;

import grid.util.Pathfinder;
import model.AgentInfo;
import service.AgentDB;
import simulations.Simulation;

public class GridWorld extends Artifact {
    private static final Logger logger = Logger.getLogger(GridWorld.class.getName());

    private GridView view;
    private AgentDB agentDB;
    private Simulation simulation;

    void init(int size, int corralWidth, int corralHeight, boolean drawCoords) {
        agentDB = new AgentDB();
        GridModel.create(size, corralWidth, corralHeight, agentDB);
        commonInit(drawCoords);
    }

    void init(String filePath, boolean drawCoords) {
        agentDB = new AgentDB();
        GridModel.create(filePath, agentDB);
        commonInit(drawCoords);
    }

    void commonInit(boolean drawCoords) {
        GridModel model = GridModel.getInstance();
        view = new GridView(model, agentDB, drawCoords);
    }

    @OPERATION
    void nextStep(int targetX, int targetY, OpFeedbackParam<Integer> newX, OpFeedbackParam<Integer> newY) {
        AgentInfo agent = agentDB.getAgentById(this.getCurrentOpAgentId().getLocalId());
        GridModel model = GridModel.getInstance();
        Pathfinder pathfinder = Pathfinder.getInstance(agent.getAgentType());
        Location startPos = model.getAgPos(agent.getCartagoId());
        Location targetPos = new Location(targetX, targetY);
        Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
        logger.info("nextStep called by " + agent.getJasonId() + " from " + startPos + " to " + targetPos
                + " calced next move -> "
                + nextPos);
        moveTo(agent, nextPos, newX, newY);
    }

    private void moveTo(AgentInfo agent, Location location, OpFeedbackParam<Integer> newX,
            OpFeedbackParam<Integer> newY) {
        GridModel model = GridModel.getInstance();

        // Technically the pathfinder should only calculate valid moves, but we might run into some concurrency issues
        // if (model.isFree(location)) {
        if (!model.getObstacleMap().isObstacle(location, agent.getAgentType())) {
            // logger.info("move successful");
            int agentCartagoId = agent.getCartagoId();
            model.setAgPos(agentDB.getAgentById(agentCartagoId), location);
            newX.set(location.x);
            newY.set(location.y);
            signal("mapChanged");
        } else {
            logger.warning("MOVE FAILED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            failed("move_failed");
        }
    }

    @OPERATION
    private void initAgent(String name, OpFeedbackParam<Integer> X, OpFeedbackParam<Integer> Y) {
        AgentInfo agent = agentDB.addAgent(this.getCurrentOpAgentId().getLocalId(), name);
        Location loc = GridModel.getInstance().initAgent(agent);
        X.set(loc.x);
        Y.set(loc.y);
        moveTo(agent, loc, X, Y);
    }

    @OPERATION
    private void startSimulation() {
        simulation = new Simulation();
        simulation.start();
    }

    @OPERATION
    private void endSimulation() {
        simulation.end();
    }
}