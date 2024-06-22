package jia;

import java.util.*;

import org.apache.commons.math3.linear.*;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import grid.GridModel;
import grid.util.Pathfinder;
import util.PropertiesLoader;

public class get_next_pos extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        //ts.getLogger().info("--------------'get_next_pos' ");                                                                                         // DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer maxNumberRecalculations = loader.getProperty("hound_max_recalc_new_target_pos", Integer.class);
        Integer keepDistanceToSheep = loader.getProperty("hound_keep_distance_to_sheep", Integer.class);

        int myX = (int) ((NumberTerm) args[0]).solve();
        int myY = (int) ((NumberTerm) args[1]).solve();
        var myLoc = new Location(myX, myY);

        int evasionX = (int) ((NumberTerm) args[2]).solve(); // [-1,0,1] signals if evasion shall be in +x or -x direction
        int evasionY = (int) ((NumberTerm) args[3]).solve();

        int targetX = (int) ((NumberTerm) args[4]).solve();
        int targetY = (int) ((NumberTerm) args[5]).solve();
        var targetLoc = new Location(targetX, targetY);

        Location validTarget = calculateValidTarget(ts, model, maxNumberRecalculations, keepDistanceToSheep, myLoc,
                targetLoc, evasionX, evasionY);
        //ts.getLogger().info("--------------'get_next_pos' next Line: Pathfinder.getInstance");                                                        // DEBUG
        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.HOUND);
        //ts.getLogger().info("--------------'get_next_pos' next Line: pathfinder.excludeObjects");                                                     // DEBUG
        pathfinder.excludeObjects(myLoc, GridModel.SHEEP, keepDistanceToSheep);
        Location nextPos = pathfinder.getNextPosition(myLoc, validTarget);
        /*ts.getLogger().info("--------------'get_next_pos' valid Target: (" + validTarget.x + "," + validTarget.y
                + ") Next_Pos: (" + nextPos.x + "," + nextPos.y + ")");  */                                                                               // DEBUG
        return un.unifies(args[6], new NumberTermImpl(nextPos.x))
                && un.unifies(args[7], new NumberTermImpl(nextPos.y));
    }

    private Location calculateValidTarget(TransitionSystem ts, GridModel model, Integer maxNumberRecalculations,
            Integer keepDistanceToSheep, Location myLoc, Location originalTarget,
            int evasionX, int evasionY) {
        //ts.getLogger().info("--------------'calculateValidTarget'");
        Queue<Location> locsToProcess = new ArrayDeque<Location>();
        locsToProcess.add(originalTarget);
        int numberRecalculations = 0;

        while (!locsToProcess.isEmpty()) {
            // avoid StackOverflow
            numberRecalculations++;
            //ts.getLogger().info("--------------'calculateValidTarget' numberRecalculations++");
            if (numberRecalculations > maxNumberRecalculations) {
                /*ts.getLogger()
                        .info("--------------'calculateValidTarget' no valid target: reached limit of recalculations"); */                                  //DEBUG
                return myLoc;
            }

            Location targetToProcess = locsToProcess.poll();
            /*ts.getLogger()
                    .info("--------------'calculateValidTarget' new target to process: " + targetToProcess.toString());   */                                //DEBUG

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
                HashSet<Location> evasionTargets = new HashSet<>();
                evasionTargets.add(new Location(targetToProcess.x + evasionX, targetToProcess.y));
                evasionTargets.add(new Location(targetToProcess.x, targetToProcess.y + evasionY));
                evasionTargets.add(new Location(targetToProcess.x + evasionX, targetToProcess.y + evasionY));

                for (var loc : evasionTargets) {
                    if (model.inGrid(loc)) {
                        locsToProcess.add(loc);
                        /* ts.getLogger()
                        .info("--------------'calculateValidTarget' added Target based on evasion: " + loc.toString()); */                       //DEBUG
                    }
                }
            }
        }

        //ts.getLogger().info("--------------'calculateValidTarget' no valid target: no places to process");                                    //DEBUG
        return myLoc;
    }
}
