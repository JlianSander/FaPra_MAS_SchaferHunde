package model;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

import jason.environment.grid.Location;

public class State implements Serializable {
    private static final long serialVersionUID = 1L;

    private final Location agentLoc;
    private final List<Location> nearbySheepPositions;
    private final List<Location> nearbyHoundPositions;
    private final int sheepAmountLeft;

    public State(Location agentLoc, List<Location> nearbySheepPositions,
            List<Location> nearbyHoundPositions,
            int sheepAmountLeft) {
        this.agentLoc = agentLoc;
        this.nearbySheepPositions = nearbySheepPositions;
        this.nearbyHoundPositions = nearbyHoundPositions;
        this.sheepAmountLeft = sheepAmountLeft;
    }

    public Location getAgentLoc() {
        return agentLoc;
    }

    public List<Location> getNearbySheepPositions() {
        return nearbySheepPositions;
    }

    public int getSheepCount() {
        return sheepAmountLeft;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        State state = (State) o;
        // return agentLoc.equals(state.agentLoc) &&
        //         sheepAmountLeft == state.sheepAmountLeft &&
        //         Objects.equals(nearbySheepPositions, state.nearbySheepPositions) &&
        //         Objects.equals(nearbyHoundPositions, state.nearbyHoundPositions);

        return agentLoc.equals(state.agentLoc);
    }

    @Override
    public int hashCode() {
        // return Objects.hash(agentLoc, nearbySheepPositions, nearbyHoundPositions, sheepAmountLeft);
        return Objects.hash(agentLoc);
    }
}