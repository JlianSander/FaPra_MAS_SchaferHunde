package model;

public class AgentInfo {
    private int agentType;
    private int cartagoId;
    private String jasonId;
    private int shortName;

    public AgentInfo(int agentType, int cartagoId, String jasonId) {
        this.agentType = agentType;
        this.cartagoId = cartagoId;
        this.jasonId = jasonId;

        String numberPart = jasonId.replaceAll("\\D+$", "").replaceAll("\\D", "");
        shortName = numberPart.isEmpty() ? 0 : Integer.parseInt(numberPart);
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

    public int getShortName() {
        return shortName;
    }
}
