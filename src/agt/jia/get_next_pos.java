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
        //ts.getLogger().info("--------------'get_next_pos' ");                                                                     //DEBUG
        GridModel model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer maxNumberCalls = loader.getProperty("hound_max_calls_new_target_pos", Integer.class);
        Integer keepDistanceToSheep = loader.getProperty("hound_keep_distance_to_sheep", Integer.class);

        int myX = (int) ((NumberTerm) args[0]).solve();
        int myY = (int) ((NumberTerm) args[1]).solve();
        var myLoc = new Location(myX, myY);

        int targetX = (int) ((NumberTerm) args[2]).solve();
        int targetY = (int) ((NumberTerm) args[3]).solve();
        var targetLoc = new Location(targetX, targetY);

        Location validTarget = calculateValidTargetRecursiv(model, maxNumberCalls, keepDistanceToSheep, myLoc, targetLoc, 0);
        //ts.getLogger().info("--------------'get_next_pos' next Line: Pathfinder.getInstance");                                                                        //DEBUG
        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.HOUND);
        //ts.getLogger().info("--------------'get_next_pos' next Line: pathfinder.excludeObjects");                                                                     //DEBUG
        pathfinder.excludeObjects(myLoc, GridModel.SHEEP, keepDistanceToSheep);
        Location nextPos = pathfinder.getNextPosition(myLoc, validTarget);
        //ts.getLogger().info("--------------'get_next_pos' valid Target: (" + validTarget.x + "," + validTarget.y  + ") Next_Pos: (" + nextPos.x + "," + nextPos.y + ")");           //DEBUG
        return un.unifies(args[4], new NumberTermImpl(nextPos.x))
                    && un.unifies(args[5], new NumberTermImpl(nextPos.y));
    }

    private Location calculateValidTargetRecursiv(GridModel model, Integer maxNumberCalls, Integer keepDistanceToSheep, Location myLoc, Location desiredTarget, int numberCalls){
        //TERMINATION condition
        if(desiredTarget.equals(myLoc) || !model.inGrid(desiredTarget) || numberCalls > maxNumberCalls){
            // return start location, so that agent does not move
            return myLoc;
        }

        var sheepNearBy = model.getNeighborhood(desiredTarget, keepDistanceToSheep, loc -> {
            List<Integer> objects = model.getObjectsAt(loc);
            return objects.contains(GridModel.SHEEP) ;
        });
        
        if(model.isFree(desiredTarget) && sheepNearBy.isEmpty()){
            return desiredTarget;
        }else{
            //target position is not valid and needs to change
            //calculate a new target position, which is supposedly valid
            Location calculatedNewTarget;
            
            if(!model.isFree(desiredTarget)){
                //target position itself is not valid    
                //calculate direction towards the target
                RealVector directionToTarget = MatrixUtils.createRealVector(new double[] {
                    desiredTarget.x - myLoc.x,
                    desiredTarget.y - myLoc.y
                });
    
                if( directionToTarget.getEntry(0) != 0 || directionToTarget.getEntry(1) != 0){
                    directionToTarget = directionToTarget.unitVector();
                }
                //invert direction
                RealVector directionAwayFromTarget =  directionToTarget.mapMultiply(-1);
    
                //calculate new target position
                calculatedNewTarget = new Location(
                    desiredTarget.x
                        + (int) Math.round(
                            directionAwayFromTarget.getEntry(0) * 1),
                    desiredTarget.y 
                        + (int) Math.round(
                            directionAwayFromTarget.getEntry(1) * 1));
    
            }else{
                //sheeps are too close to target position
                // focus on first sheep in list
                Location locSheep = sheepNearBy.get(0); 

                //calculate direction towards the first sheep
                RealVector directionToSheep = MatrixUtils.createRealVector(new double[] {
                    locSheep.x - desiredTarget.x,
                    locSheep.y - desiredTarget.y
                });

                if( directionToSheep.getEntry(0) != 0 || directionToSheep.getEntry(1) != 0){
                    directionToSheep = directionToSheep.unitVector();
                }
                //invert direction
                RealVector directionAwayFromSheep =  directionToSheep.mapMultiply(-1);

                //calculate new target position
                calculatedNewTarget = new Location(
                    desiredTarget.x
                        + (int) Math.round(
                            directionAwayFromSheep.getEntry(0) * 1),
                    desiredTarget.y 
                        + (int) Math.round(
                            directionAwayFromSheep.getEntry(1) * 1));
            }

            //RECURSIVE CALL
            //call method again to check new target position
            return calculateValidTargetRecursiv(model, maxNumberCalls, keepDistanceToSheep, myLoc, calculatedNewTarget, numberCalls++);
        }
    }
}
