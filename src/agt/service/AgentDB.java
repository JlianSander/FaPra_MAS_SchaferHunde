package service;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import grid.GridModel;
import model.AgentInfo;

public class AgentDB {
    private static volatile AgentDB instance;
    private List<AgentInfo> agents;

    private AgentDB() {
        this.agents = new CopyOnWriteArrayList<>();
    }

    public static AgentDB getInstance() {
        if (instance == null) {
            synchronized (AgentDB.class) {
                if (instance == null) {
                    instance = new AgentDB();
                }
            }
        }
        return instance;
    }

    public AgentInfo addAgent(int cartagoId, String name) {
        int agentType = -1;
        if (name.toLowerCase().contains("sheep")) {
            agentType = GridModel.SHEEP;
        } else if (name.toLowerCase().contains("hound")) {
            agentType = GridModel.HOUND;
        } else {
            throw new IllegalArgumentException("Invalid agent name");
        }
        AgentInfo agent = new AgentInfo(agentType, cartagoId, name);
        agents.add(agent);
        return agent;
    }

    public boolean removeAgent(int cartagoId) {
        return agents.removeIf(agent -> agent.getCartagoId() == cartagoId);
    }

    public List<AgentInfo> getAllAgents() {
        return agents;
    }

    public AgentInfo getAgentByCartagoId(int cartagoId) {
        return agents.stream()
                .filter(agent -> agent.getCartagoId() == cartagoId)
                .findFirst()
                .orElse(null);
    }

    public AgentInfo getAgentByJasonId(String jasonId) {
        return agents.stream()
                .filter(agent -> agent.getJasonId().equals(jasonId))
                .findFirst()
                .orElse(null);
    }

    public AgentInfo getAgentByLocation(int x, int y) {
        return agents.stream()
                .filter(agent -> agent.getCartagoId() == GridModel.getInstance().getAgAtPos(x, y))
                .findFirst()
                .orElse(null);
    }
}