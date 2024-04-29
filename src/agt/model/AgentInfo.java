package model;

public class AgentInfo {
    public enum AgentType {
        SHEEP, HOUND
    }

    private AgentType type;
    private int cartagoId;
    private String jasonId;

    public AgentInfo(AgentType type, int cartagoId, String jasonId) {
        this.type = type;
        this.cartagoId = cartagoId;
        this.jasonId = jasonId;
    }

    public AgentType getType() {
        return type;
    }

    public int getCartagoId() {
        return cartagoId;
    }

    public String getJasonId() {
        return jasonId;
    }
}
