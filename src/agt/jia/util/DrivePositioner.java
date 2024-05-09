package jia.util;

import jason.environment.grid.Location;

import grid.GridModel;
import jason.environment.grid.Area;

public class DrivePositioner {

    public static Location positionSingleAgent(SwarmManipulator swarm, Area corral) {
        GridModel model = GridModel.getInstance();

        //get position, where the swarm is to drive to        
        var swarmTargetLoc = swarm.getNextPositionTo(corral.center());

        //position agent behind swarm
        int[] direction_swarm = {
                swarmTargetLoc.x - swarm.center().x,
                swarmTargetLoc.y - swarm.center().y
        };

        var agentPos = new Location(
                swarm.center().x - direction_swarm[0] * (swarm.radius() + 1),
                swarm.center().y - direction_swarm[1] * (swarm.radius() + 1));

        //ensure to stay on map
        agentPos = new Location(
                agentPos.x > model.getWidth() - 1 ? model.getWidth() - 1 : agentPos.x,
                agentPos.y > model.getHeight() - 1 ? model.getHeight() - 1 : agentPos.y);
        return agentPos;
    }
}
