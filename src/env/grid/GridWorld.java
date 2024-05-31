package grid;

import java.util.logging.Logger;

import cartago.*;
import jason.environment.grid.Location;

import grid.util.Pathfinder;
import model.AgentInfo;
import model.ScenarioInfo;
import service.AgentDB;
import util.PropertiesLoader;
import simulations.Simulation;

public class GridWorld extends Artifact {
    private static final Logger logger = Logger.getLogger(GridWorld.class.getName());

    private GridView view;
    private AgentDB agentDB;
    private Simulation simulation;
    private ScenarioInfo scenarioInfo;

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

    private void commonInit(boolean drawCoords) {
        GridModel model = GridModel.getInstance();
        view = new GridView(model, agentDB, drawCoords);

        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer sheepWaitTime = loader.getProperty("sheep_wait_duration", Integer.class);
        Double houndWaitRatio = loader.getProperty("hound_wait_ratio", Double.class);
        Integer houndWaitTime = (int) (sheepWaitTime * houndWaitRatio);
        scenarioInfo = new ScenarioInfo(sheepWaitTime, houndWaitTime, houndWaitRatio);
    }

    @OPERATION
    void nextStep(int targetX, int targetY, OpFeedbackParam<Integer> newX, OpFeedbackParam<Integer> newY) {
        AgentInfo agent = agentDB.getAgentByCartagoId(this.getCurrentOpAgentId().getLocalId());
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

        if (!model.getObstacleMap().isObstacle(location, agent.getAgentType())) {
            int agentCartagoId = agent.getCartagoId();
            model.setAgPos(agentDB.getAgentByCartagoId(agentCartagoId), location);
            newX.set(location.x);
            newY.set(location.y);
            signal("mapChanged");
        } else {
            logger.warning("MOVE FAILED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            failed("move_failed");
        }
    }

    @OPERATION
    void initAgent(String name, OpFeedbackParam<Integer> X, OpFeedbackParam<Integer> Y,
            OpFeedbackParam<Integer> waitTime) {
        AgentInfo agent = agentDB.addAgent(this.getCurrentOpAgentId().getLocalId(), name);
        scenarioInfo.addAgent(agent);
        waitTime.set(agent.getAgentType() == GridModel.SHEEP ? scenarioInfo.getSheepWaitTime()
                : scenarioInfo.getHoundWaitTime());
        Location loc = GridModel.getInstance().initAgent(agent);
        X.set(loc.x);
        Y.set(loc.y);
        moveTo(agent, loc, X, Y);
    }

    @OPERATION
    void sheepCaptured() {
        if (simulation == null) {
            return;
        }

        AgentInfo agent = agentDB.getAgentByCartagoId(this.getCurrentOpAgentId().getLocalId());
        simulation.sheepCaptured(agent);
        if (scenarioInfo.getTotalSheepCount() == simulation.getSheepCapturedCount()) {
            signal("simulationEnded");
        }
    }

    @OPERATION
    void startSimulation() {
        simulation = new Simulation(scenarioInfo);
        simulation.start();
    }

    @OPERATION
    void endSimulation() {
        simulation.end();
    }
}