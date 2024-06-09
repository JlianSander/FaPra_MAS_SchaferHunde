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
        Integer hound_distance_to_swarm = loader.getProperty("hound_keep_distance_to_swarm", Integer.class);

        int myX = (int) ((NumberTerm) args[0]).solve();
        int myY = (int) ((NumberTerm) args[1]).solve();
        var myLoc = new Location(myX, myY);

        int targetX = (int) ((NumberTerm) args[2]).solve();
        int targetY = (int) ((NumberTerm) args[3]).solve();
        var targetLoc = new Location(targetX, targetY);

        int spacing = (int) ((NumberTerm) args[4]).solve();

        //TERMINATION condition
        if(targetLoc.equals(myLoc) || !model.inGrid(targetLoc)){
            // return start location, so that agent does not move
            return un.unifies(args[5], new NumberTermImpl(myX))
                && un.unifies(args[6], new NumberTermImpl(myY));
        }

        var sheepNearBy = model.getNeighborhood(targetLoc, hound_distance_to_swarm, loc -> {
            List<Integer> objects = model.getObjectsAt(loc);
            return objects.contains(GridModel.SHEEP) ;
        });
        
        if(!model.isFree(targetLoc) || !sheepNearBy.isEmpty()){
            //target position is no valid and needs to change
            if(!model.isFree(targetLoc)){
                //target position itself is not valid    
                //calculate direction towards the target
                RealVector directionToTarget = MatrixUtils.createRealVector(new double[] {
                    targetLoc.x - myLoc.x,
                    targetLoc.y - myLoc.y
                });
    
                if( directionToTarget.getEntry(0) != 0 || directionToTarget.getEntry(1) != 0){
                    directionToTarget = directionToTarget.unitVector();
                }
                //invert direction
                RealVector directionAwayFromTarget =  directionToTarget.mapMultiply(-1);
    
                //calculate new target position
                targetLoc = new Location(
                    targetLoc.x
                        + (int) Math.round(
                            directionAwayFromTarget.getEntry(0) * 1),
                    targetLoc.y 
                        + (int) Math.round(
                            directionAwayFromTarget.getEntry(1) * 1));
    
            }else{
                //sheeps are too close to target position
                // focus on first sheep in list
                Location locSheep = sheepNearBy.get(0); 

                //calculate direction towards the first sheep
                RealVector directionToSheep = MatrixUtils.createRealVector(new double[] {
                    locSheep.x - targetLoc.x,
                    locSheep.y - targetLoc.y
                });

                if( directionToSheep.getEntry(0) != 0 || directionToSheep.getEntry(1) != 0){
                    directionToSheep = directionToSheep.unitVector();
                }
                //invert direction
                RealVector directionAwayFromSheep =  directionToSheep.mapMultiply(-1);

                //calculate new target position
                targetLoc = new Location(
                    targetLoc.x
                        + (int) Math.round(
                            directionAwayFromSheep.getEntry(0) * 1),
                    targetLoc.y 
                        + (int) Math.round(
                            directionAwayFromSheep.getEntry(1) * 1));
            }

            //change arguments for recursive call
            Term[] newArgs = Arrays.copyOf(args, args.length);
            newArgs[2] = new NumberTermImpl(targetLoc.x);
            newArgs[3] = new NumberTermImpl(targetLoc.y);

            //RECURSIVE CALL
            //call method again to check new target position
            return execute(ts, un, newArgs);
        }

        //ts.getLogger().info("--------------'get_next_pos' next Line: Pathfinder.getInstance");                                                                     //DEBUG
        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.HOUND);
        //ts.getLogger().info("--------------'get_next_pos' next Line: pathfinder.excludeObjects");                                                                     //DEBUG
        pathfinder.excludeObjects(myLoc, GridModel.SHEEP, spacing);
        Location nextPos = pathfinder.getNextPosition(myLoc, targetLoc);
        //ts.getLogger().info("--------------'get_next_pos' Next_Pos: (" + nextPos.x + "," + nextPos.y + ")");                                     //DEBUG

        return un.unifies(args[5], new NumberTermImpl(nextPos.x))
                && un.unifies(args[6], new NumberTermImpl(nextPos.y));
    }

}
