package jia.util;

import org.apache.commons.math3.linear.*;

import jason.environment.grid.Location;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Area;

import grid.GridModel;
import util.PropertiesLoader;

public class DriveStrategy1 implements IDrivePositioner {

    public Location calculateAgentPosition(TransitionSystem ts, SwarmManipulator swarm, Area corral, int positionNumber) {
        //ts.getLogger().info("--------------'positionAgent' positionNumber: " + positionNumber);                                                                       //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer hound_distance_to_swarm = loader.getProperty("hound_keep_distance_to_swarm", Integer.class);
        Double angle_incr = loader.getProperty("hound_driving_position_angle_increment", Double.class);
        //ts.getLogger().info("--------------'positionAgent' angle_incr: " + angle_incr);                                                                               //DEBUG

        //get position, where the swarm is to drive to        
        //ts.getLogger().info("--------------'positionAgent' Swarm.Center: (" + swarm.center().x + "," + swarm.center().y + ")");                                     //DEBUG
        var swarmTargetLoc = swarm.getNextPositionTo(corral.center());
        //ts.getLogger().info("--------------'positionAgent' Swarm_Next_Pos: (" + swarmTargetLoc.x + "," + swarmTargetLoc.y + ")");                                     //DEBUG

        //get direction of the swarms desired movements
        RealVector direction_swarm = MatrixUtils.createRealVector(new double[] {
                swarmTargetLoc.x - swarm.center().x,
                swarmTargetLoc.y - swarm.center().y
        });
        //ts.getLogger().info("--------------'positionAgent' swarm_direction not normalized: [" + direction_swarm.getEntry(0) + "][" + direction_swarm.getEntry(1) + "]");         //DEBUG
        if( direction_swarm.getEntry(0) != 0 || direction_swarm.getEntry(1) != 0){
            direction_swarm = direction_swarm.unitVector();
            //ts.getLogger().info("--------------'positionAgent' swarm_direction: [" + direction_swarm.getEntry(0) + "][" + direction_swarm.getEntry(1) + "]");                      //DEBUG
        }
        

        //calculate angle depending on the positionNumber
        double angle = (3 - positionNumber) * angle_incr;
        //ts.getLogger().info("--------------'positionAgent' calculated angle: " + angle);                                                                             //DEBUG
        angle = Math.toRadians(angle);

        //create rotation matrix
        double[][] rotation_entries = {
                { Math.cos(angle), -1 * Math.sin(angle) },
                { Math.sin(angle), Math.cos(angle) }
        };
        // Matrix rotation = new Matrix(rotation_entries);
        RealMatrix rotation = MatrixUtils.createRealMatrix(rotation_entries);
        //ts.getLogger().info("--------------'positionAgent' rotation: [" + rotation_entries[0][0] + " " + rotation_entries[0][1] + "][" + rotation_entries[1][0] + " " + rotation_entries[1][1] + "]");   //DEBUG

        //rotate direction according to angle
        RealVector direction_swarm_rotated = rotation.operate(direction_swarm);
        //ts.getLogger().info("--------------'positionAgent' swarm_direction rotated: [" + direction_swarm_rotated.getEntry(0) + "][" + direction_swarm_rotated.getEntry(1) + "]");                 //DEBUG

        //calculate agents position behind the swarm
        var agentPos = new Location(
                swarm.center().x
                        - (int) Math.round(
                                direction_swarm_rotated.getEntry(0) * (swarm.radius() + hound_distance_to_swarm)),
                swarm.center().y - (int) Math
                        .round(direction_swarm_rotated.getEntry(1) * (swarm.radius() + hound_distance_to_swarm)));
        //ts.getLogger().info("--------------'positionAgent' agent_pos before map limit: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG

        //ensure to stay on map
        agentPos = new Location(
                stay_within_limit(agentPos.x, model.getWidth() - 1),
                stay_within_limit(agentPos.y, model.getHeight() - 1));

        //ts.getLogger().info("--------------'positionAgent' agent_pos after map limit: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG

        //prevent to position agent on spot of swarm center in case the position got changed because of map limits
        if (agentPos.equals(swarm.center())) {
            if (agentPos.x != model.getWidth() - 1) {
                agentPos = new Location(agentPos.x + 1, agentPos.y);
            } else if (agentPos.y != model.getHeight() - 1) {
                agentPos = new Location(agentPos.x, agentPos.y + 1);
            } else {
                agentPos = new Location(agentPos.x - 1, agentPos.y - 1);
            }
        }

        //ts.getLogger().info("--------------'positionAgent' agent_pos final: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG

        return agentPos;
    }

    private static int stay_within_limit(int actVal, int maxLimit) {
        int output = actVal > 0 ? actVal : 0;
        return output < maxLimit ? output : maxLimit;
    }
}