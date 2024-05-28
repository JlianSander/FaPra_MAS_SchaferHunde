package model;

public class SimulationInfo {
    private int totalSheepCount;
    private int totalHoundCount;
    private int sheepWaitTime;
    private int houndWaitTime;
    private double houndRelativeWaitTime;

    public SimulationInfo(int sheepWaitTime, int houndWaitTime, double houndRelativeWaitTime) {
        this.sheepWaitTime = sheepWaitTime;
        this.houndWaitTime = houndWaitTime;
        this.houndRelativeWaitTime = houndRelativeWaitTime;
    }

    public void addSheep() {
        totalSheepCount++;
    }

    public void addHound() {
        totalHoundCount++;
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
