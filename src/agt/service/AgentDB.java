package service;

import java.util.ArrayList;
import java.util.List;

import model.AgentInfo;
import model.AgentInfo.AgentType;

public class AgentDB {
    private List<AgentInfo> agents;

    public AgentDB() {
        this.agents = new ArrayList<>();
    }

    public void addAgent(int id, String name) {
        AgentType type = null;
        if (name.contains("sheep")) {
            type = AgentType.SHEEP;
        } else if (name.contains("hound")) {
            type = AgentType.HOUND;
        } else {
            throw new IllegalArgumentException("Invalid agent name");
        }
        agents.add(new AgentInfo(type, id, name));
    }

    public List<AgentInfo> getAllAgents() {
        return agents;
    }

    public AgentInfo getAgentById(int id) {
        for (AgentInfo agent : agents) {
            if (agent.getCartagoId() == id) {
                return agent;
            }
        }
        return null;
    }

    public AgentInfo getAgentByJasonId(String id) {
        for (AgentInfo agent : agents) {
            if (agent.getJasonId().equals(id)) {
                return agent;
            }
        }
        return null;
    }
}
