package model;

import grid.GridModel;

public class ScenarioInfo {
    private int totalSheepCount;
    private int totalHoundCount;
    private int sheepWaitTime;
    private int houndWaitTime;
    private double houndRelativeWaitTime;

    public ScenarioInfo(int sheepWaitTime, int houndWaitTime, double houndRelativeWaitTime) {
        this.sheepWaitTime = sheepWaitTime;
        this.houndWaitTime = houndWaitTime;
        this.houndRelativeWaitTime = houndRelativeWaitTime;
    }

    public void addAgent(AgentInfo agent) {
        switch (agent.getAgentType()) {
            case GridModel.SHEEP:
                totalSheepCount++;
                break;
            case GridModel.HOUND:
                totalHoundCount++;
                break;
            default:
                throw new IllegalArgumentException("Invalid agent type: " + agent.getAgentType());
        }
    }

    public int getTotalSheepCount() {
        return totalSheepCount;
    }

    public int getTotalHoundCount() {
        return totalHoundCount;
    }

    public int getSheepWaitTime() {
        return sheepWaitTime;
    }

    public int getHoundWaitTime() {
        return houndWaitTime;
    }

    public double getHoundRelativeWaitTime() {
        return houndRelativeWaitTime;
    }
}
