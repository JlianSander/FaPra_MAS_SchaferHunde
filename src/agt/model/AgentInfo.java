package model;

public class AgentInfo {
    private int agentType;
    private int cartagoId;
    private String jasonId;

    public AgentInfo(int agentType, int cartagoId, String jasonId) {
        this.agentType = agentType;
        this.cartagoId = cartagoId;
        this.jasonId = jasonId;
    }

    public int getAgentType() {
        return agentType;
    }

    public int getCartagoId() {
        return cartagoId;
    }

    public String getJasonId() {
        return jasonId;
    }
}
