package model;

import jason.environment.grid.Location;

public class AgentInfo {
    public enum Facing {
        LEFT, RIGHT
    };

    private int agentType;
    private int cartagoId;
    private String jasonId;
    private int shortName;
    private Facing facing = Facing.RIGHT;

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

    public Facing getFacing() {
        return facing;
    }

    public void onAgentMoved(Location old, Location current) {
        if (old.x < current.x) {
            facing = Facing.RIGHT;
        } else if (old.x > current.x) {
            facing = Facing.LEFT;
        }
    }
}
