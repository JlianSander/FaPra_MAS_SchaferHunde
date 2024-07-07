package jia.util;

import java.util.ArrayDeque;
import java.util.HashSet;
import java.util.List;
import java.util.Queue;

import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealVector;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;
import util.PropertiesLoader;

public class ValidatorPos {

    public static Location ensurePosValid(TransitionSystem ts, Location myLoc, Location targetPos, RealVector evasionDirection, int offsetToSheep) {
        /*ts.getLogger().info("--------------'ValidatorPos::ensurePosValid' myLoc:" + myLoc.toString() + " targetPos:" + targetPos.toString() 
        + " evasionDirection:" + evasionDirection.toString() + " offsetSheep:" + offsetToSheep);   */                                                             //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer maxNumberRecalculations = loader.getProperty("hound_max_recalc_new_target_pos", Integer.class);

        Location posOnMap = ensurePosOnMap(ts, model, targetPos);
        //ts.getLogger().info("--------------'ValidatorPos::ensurePosValid' posOnMap: " + posOnMap.toString());                                                 //DEBUG
        Location result  = calculateValidTarget(ts, model, maxNumberRecalculations, offsetToSheep, myLoc, posOnMap, evasionDirection, 1);
        //ts.getLogger().info("--------------'ValidatorPos::ensurePosValid' result: " + result.toString());                                                     //DEBUG

        return result;
    }

    private static Location calculateValidTarget(TransitionSystem ts, GridModel model, Integer maxNumberRecalculations,
            Integer keepDistanceToSheep, Location myLoc, Location originalTarget, RealVector evasionDir, int offsetEvasion) {
        //ts.getLogger().info("--------------'calculateValidTarget'");
        Queue<Location> locsToProcess = new ArrayDeque<Location>();
        locsToProcess.add(originalTarget);
        int numberRecalculations = 0;

        while (!locsToProcess.isEmpty()) {
            // avoid StackOverflow
            numberRecalculations++;
            //ts.getLogger().info("--------------'calculateValidTarget' numberRecalculations++");
            if (numberRecalculations > maxNumberRecalculations) {
                ts.getLogger()
                        .info("--------------'get_next_pos::calculateValidTarget' no valid target: reached limit of recalculations");                                  //DEBUG
                return myLoc;
            }

            Location targetToProcess = locsToProcess.poll();           
            /*ts.getLogger()
                    .info("--------------'calculateValidTarget' new target to process: " + targetToProcess.toString());   */                                //DEBUG
            if(targetToProcess.equals(myLoc)){
                //current position of the agent is a valid position
                return myLoc;
            }

            var sheepTooCloseBy = model.getNeighborhood(targetToProcess, keepDistanceToSheep, loc -> {
                List<Integer> objects = model.getObjectsAt(loc);
                return objects.contains(GridModel.SHEEP);
            });

            if (model.isFree(targetToProcess) && sheepTooCloseBy.isEmpty()) {
                /* ts.getLogger().info("--------------'calculateValidTarget' calculated new valid target: "
                        + targetToProcess.toString());  */                                                                                                //DEBUG
                return targetToProcess;
            }
            // --------------------------------------------------
            // target position is not valid and needs to change

            //calculate new target by using direction to obstacle
            {
                Location calculatedNewTarget;

                if (!model.isFree(targetToProcess)) {
                    // target position itself is not valid
                    // calculate direction towards the target
                    RealVector directionToTarget = MatrixUtils.createRealVector(new double[] {
                            targetToProcess.x - myLoc.x,
                            targetToProcess.y - myLoc.y
                    });

                    if (directionToTarget.getEntry(0) != 0 || directionToTarget.getEntry(1) != 0) {
                        directionToTarget = directionToTarget.unitVector();
                    }
                    // invert direction
                    RealVector directionAwayFromTarget = directionToTarget.mapMultiply(-1);

                    // calculate new target position
                    calculatedNewTarget = new Location(
                            targetToProcess.x
                                    + (int) Math.round(
                                            directionAwayFromTarget.getEntry(0) * 1),
                            targetToProcess.y
                                    + (int) Math.round(
                                            directionAwayFromTarget.getEntry(1) * 1));
                } else {

                    // sheeps are too close to target position
                    // focus on first sheep in list
                    Location locSheep = sheepTooCloseBy.get(0);

                    // calculate direction towards the first sheep
                    RealVector directionToSheep = MatrixUtils.createRealVector(new double[] {
                            locSheep.x - targetToProcess.x,
                            locSheep.y - targetToProcess.y
                    });

                    if (directionToSheep.getEntry(0) != 0 || directionToSheep.getEntry(1) != 0) {
                        directionToSheep = directionToSheep.unitVector();
                    }
                    // invert direction
                    RealVector directionAwayFromSheep = directionToSheep.mapMultiply(-1);

                    // calculate new target position
                    calculatedNewTarget = new Location(
                            targetToProcess.x
                                    + (int) Math.round(
                                            directionAwayFromSheep.getEntry(0) * 1),
                            targetToProcess.y
                                    + (int) Math.round(
                                            directionAwayFromSheep.getEntry(1) * 1));
                }

                if (model.inGrid(calculatedNewTarget)) {
                    locsToProcess.add(calculatedNewTarget);
                    /* ts.getLogger()
                    .info("--------------'calculateValidTarget' added Target based on direction: " + calculatedNewTarget.toString()); */        //DEBUG
                }
            }

            // calculate other target options based on favourable evasion direction
            {
                var evasionTargets = new HashSet<Location>();
                evasionTargets.add(GeometryCalculator.translateInDir(ts, targetToProcess, evasionDir, offsetEvasion));
                // evasionTargets.add(new Location(targetToProcess.x + evasionX, targetToProcess.y));
                // evasionTargets.add(new Location(targetToProcess.x, targetToProcess.y + evasionY));
                // evasionTargets.add(new Location(targetToProcess.x + evasionX, targetToProcess.y + evasionY));

                for (var loc : evasionTargets) {
                    if (model.inGrid(loc)) {
                        locsToProcess.add(loc);
                        /* ts.getLogger()
                        .info("--------------'calculateValidTarget' added Target based on evasion: " + loc.toString()); */                       //DEBUG
                    }
                }
            }
        }

        ts.getLogger().info("--------------'get_next_pos::calculateValidTarget' no valid target: no places to process");                                    //DEBUG
        return myLoc;
    }

    private static Location ensurePosOnMap(TransitionSystem ts, GridModel model, Location targetPos) {
        //ts.getLogger().info("--------------'ValidatorPos::ensurePosOnMap' targetPos: " + targetPos.toString());                                    //DEBUG
        //ensure to stay on map
        Location validPos = new Location(
                stay_within_limit(ts, targetPos.x, model.getWidth() - 1),
                stay_within_limit(ts, targetPos.y, model.getHeight() - 1));

        //ts.getLogger().info("--------------'ValidatorPos::ensurePosOnMap' validPos: " + validPos.toString());                                    //DEBUG
        return validPos;
    }

    private static int stay_within_limit(TransitionSystem ts, int actVal, int maxLimit) {
        //ts.getLogger().info("--------------'ValidatorPos::stay_within_limit' actVal: " + actVal);                                    //DEBUG
        //ts.getLogger().info("--------------'ValidatorPos::stay_within_limit' maxLimit: " + maxLimit);                                    //DEBUG
        int output = actVal > 0 ? actVal : 0;
        output =  output < maxLimit ? output : maxLimit;
        //ts.getLogger().info("--------------'ValidatorPos::stay_within_limit' output: " + output);                                    //DEBUG
        return output;
    }
}
