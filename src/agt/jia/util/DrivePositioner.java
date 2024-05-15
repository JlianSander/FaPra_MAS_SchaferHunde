package agt.jia.util;

import jason.environment.grid.Location;

import java.util.Arrays;
import java.util.Vector;

import grid.GridModel;
import grid.util.Pathfinder;
import jason.environment.grid.Area;

public class DrivePositioner {
    
    public static Location positionSingleAgent(SwarmManipulator swarm, Area corral){
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
            swarm.center().y - direction_swarm[1] * (swarm.radius() + 1)
        );

        //ensure to stay on map
        agentPos = new Location(
            stay_within_limit(agentPos.x, model.getWidth() - 1),
            stay_within_limit(agentPos.y, model.getHeight() - 1)
        );
        return agentPos;
    }

    private static int stay_within_limit(int actVal, int maxLimit){
        int output = actVal > 0 ? actVal : 0;
        return output < maxLimit ? output : maxLimit;
    }
}
