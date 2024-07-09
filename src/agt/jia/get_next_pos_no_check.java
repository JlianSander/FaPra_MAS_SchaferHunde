package jia;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.AgentUtil;
import grid.GridModel;
import grid.util.Pathfinder;

public class get_next_pos_no_check extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        var myLoc = AgentUtil.getAgentPositionFromTs(ts);

        int targetX = (int) ((NumberTerm) args[0]).solve();
        int targetY = (int) ((NumberTerm) args[1]).solve();
        var targetLoc = new Location(targetX, targetY);

        if(targetLoc.equals(myLoc)){
            return un.unifies(args[2], new NumberTermImpl(myLoc.x))
                && un.unifies(args[3], new NumberTermImpl(myLoc.y));
        }

        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.HOUND);
        Location nextPos = pathfinder.getNextPosition(myLoc, targetLoc);
        ts.getLogger().info("--------------'get_next_pos_no_check' myLoc: " + myLoc.toString() + " Target: " + targetLoc.toString() + " Next_Pos: " + nextPos.toString());                                                                               // DEBUG
        return un.unifies(args[2], new NumberTermImpl(nextPos.x))
                && un.unifies(args[3], new NumberTermImpl(nextPos.y));
    }
}

