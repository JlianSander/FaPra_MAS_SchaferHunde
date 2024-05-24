package jia.util;

import jason.environment.grid.Location;
import jason.asSemantics.TransitionSystem;

import grid.GridModel;
import jason.environment.grid.Area;

public class DrivePositioner {

    public static Location positionSingleAgent(TransitionSystem ts, SwarmManipulator swarm, Area corral) {
        GridModel model = GridModel.getInstance();

        //get position, where the swarm is to drive to        
        var swarmTargetLoc = swarm.getNextPositionTo(corral.center());

        //TODO: use beliefs about other hound positions to choose strategie for positioning
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
            stay_within_limit(agentPos.x, model.getWidth() - 1),
            stay_within_limit(agentPos.y, model.getHeight() - 1));
        
        //prevent to position agent on spot of swarm center in case the position got changed because of map limits
        if(agentPos.equals(swarm.center())){
            if(agentPos.x != model.getWidth() - 1){
                agentPos = new Location(agentPos.x + 1, agentPos.y);
            }else if(agentPos.y != model.getHeight() - 1){
                agentPos = new Location(agentPos.x, agentPos.y + 1);
            }else{
                agentPos = new Location(agentPos.x - 1, agentPos.y - 1);
            }
        }
        
        return agentPos;
    }

    private static int stay_within_limit(int actVal, int maxLimit) {
        int output = actVal > 0 ? actVal : 0;
        return output < maxLimit ? output : maxLimit;
    }
}
