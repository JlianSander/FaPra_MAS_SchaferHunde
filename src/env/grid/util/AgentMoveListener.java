package grid.util;

import jason.environment.grid.Location;

public interface AgentMoveListener {
    void onAgentMoved(Location prevLoc, Location newLoc);
}