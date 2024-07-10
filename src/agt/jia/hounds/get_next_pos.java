package jia.hounds;

import org.apache.commons.math3.linear.*;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.hounds.ValidatorPos;
import grid.GridModel;
import grid.util.Pathfinder;
import util.PropertiesLoader;

public class get_next_pos extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        //ts.getLogger().info("--------------'get_next_pos' ");                                                                                         // DEBUG
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer keepDistanceToSheep = loader.getProperty("hound_keep_distance_to_sheep", Integer.class);

        int myX = (int) ((NumberTerm) args[0]).solve();
        int myY = (int) ((NumberTerm) args[1]).solve();
        var myLoc = new Location(myX, myY);

        int evasionX = (int) ((NumberTerm) args[2]).solve(); // [-1,0,1] signals if evasion shall be in +x or -x direction
        int evasionY = (int) ((NumberTerm) args[3]).solve();
        RealVector evasionDirection = MatrixUtils.createRealVector(new double[] { evasionX, evasionY });

        int targetX = (int) ((NumberTerm) args[4]).solve();
        int targetY = (int) ((NumberTerm) args[5]).solve();
        var targetLoc = new Location(targetX, targetY);

        if (targetLoc.equals(myLoc)) {
            return un.unifies(args[6], new NumberTermImpl(myLoc.x))
                    && un.unifies(args[7], new NumberTermImpl(myLoc.y));
        }

        Location validTarget = ValidatorPos.ensurePosValid(ts, myLoc, targetLoc, evasionDirection, keepDistanceToSheep);
        //ts.getLogger().info("--------------'get_next_pos' next Line: Pathfinder.getInstance");                                                        // DEBUG
        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.HOUND);
        //ts.getLogger().info("--------------'get_next_pos' next Line: pathfinder.excludeObjects");                                                     // DEBUG
        pathfinder.excludeObjects(myLoc, GridModel.SHEEP, keepDistanceToSheep);
        Location nextPos = pathfinder.getNextPosition(myLoc, validTarget);
        /*ts.getLogger().info("--------------'get_next_pos' valid Target: (" + validTarget.x + "," + validTarget.y
                + ") Next_Pos: (" + nextPos.x + "," + nextPos.y + ")");  */ // DEBUG
        return un.unifies(args[6], new NumberTermImpl(nextPos.x))
                && un.unifies(args[7], new NumberTermImpl(nextPos.y));
    }
}