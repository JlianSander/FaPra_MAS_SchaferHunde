package jia.util;

import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.RealVector;

import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;

public class AngleTranslation {
    public static Location translate(TransitionSystem ts, Location center, RealVector directionFromCenter, double angleInDegree, int radius){
        double angle = Math.toRadians(angleInDegree);

        //create rotation matrix
        double[][] rotation_entries = {
                { Math.cos(angle), -1 * Math.sin(angle) },
                { Math.sin(angle), Math.cos(angle) }
        };
        // Matrix rotation = new Matrix(rotation_entries);
        RealMatrix rotation = MatrixUtils.createRealMatrix(rotation_entries);
        //ts.getLogger().info("--------------'positionAgent' rotation: [" + rotation_entries[0][0] + " " + rotation_entries[0][1] + "][" + rotation_entries[1][0] + " " + rotation_entries[1][1] + "]");   //DEBUG

        //rotate direction according to angle
        RealVector direction_swarm_rotated = rotation.operate(directionFromCenter);
        //ts.getLogger().info("--------------'positionAgent' swarm_direction rotated: [" + direction_swarm_rotated.getEntry(0) + "][" + direction_swarm_rotated.getEntry(1) + "]");                 //DEBUG

         //calculate agents position behind the swarm
        var agentPos = new Location(
                center.x
                     + (int) Math.round(
                                direction_swarm_rotated.getEntry(0) * (radius)),
                center.y + (int) Math
                        .round(direction_swarm_rotated.getEntry(1) * (radius)));
        //ts.getLogger().info("--------------'positionAgent' agent_pos before map limit: (" + agentPos.x + "," + agentPos.y + ")");                                    //DEBUG

        return agentPos;
    }
}
