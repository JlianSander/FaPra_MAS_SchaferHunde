package jia.util;


import java.lang.Math.*;

import jason.environment.grid.Location;
import jason.asSemantics.TransitionSystem;
import jason.asSyntax.Literal;
import jason.asSyntax.PredicateIndicator;

import grid.GridModel;
import jason.environment.grid.Area;

import util.PropertiesLoader;
import linearalgebra.Vector;

public class DrivePositioner {

    public static Location positionAgent(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber) {
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer hound_distance_to_swarm = loader.getProperty("hound_distance_to_swarm", Integer.class);
        Double angle_incr = loader.getProperty("hound_driving_position_angle_increment", Double.class);

        //get position, where the swarm is to drive to        
        var swarmTargetLoc = swarm.getNextPositionTo(corral.center());

        //get direction of the swarms desired movements
        Vector direction_swarm = new Vector(
                swarmTargetLoc.x - swarm.center().x,
                swarmTargetLoc.y - swarm.center().y
        );
        Vector direction_swarm_normalized = direction_swarm.normalize();

        //calculate angle depending on the positionNumber
        double angle = (positionNumber - 3) * angle_incr;

        //rotate direction according to angle
        double[] direction_swarm_rotated = {
            direction_swarm_normalized.get(0) * Math.cos(angle) - direction_swarm_normalized.get(1) * Math.sin(angle),
            direction_swarm_normalized.get(0) * Math.sin(angle) + direction_swarm_normalized.get(1) * Math.cos(angle) 
        };
        
        //calculate agents position behind the swarm
        var agentPos = new Location(
            swarm.center().x - (int) Math.round(direction_swarm_rotated[0] * (swarm.radius() + hound_distance_to_swarm)),
            swarm.center().y - (int) Math.round(direction_swarm_rotated[1] * (swarm.radius() + hound_distance_to_swarm))
        );

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
