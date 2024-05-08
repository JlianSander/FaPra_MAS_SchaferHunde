package jia;

import agt.jia.util.DrivePositioner;
import agt.jia.util.SwarmManipulator;
import grid.GridModel;
import grid.util.Pathfinder;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class get_next_pos extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int myX = (int) ((NumberTerm) args[0]).solve();
        int myY = (int) ((NumberTerm) args[1]).solve();
        var myLoc = new Location(myX, myY);
        
        int targetX = (int) ((NumberTerm) args[2]).solve();
        int targetY = (int) ((NumberTerm) args[3]).solve();
        var targetLoc = new Location(targetX, targetY);

        GridModel model = GridModel.getInstance();
        var pathfinder = new Pathfinder(model);
        Location nextPos = pathfinder.getNextPosition(myLoc, targetLoc);

        return un.unifies(args[4], new NumberTermImpl(nextPos.x))
                    && un.unifies(args[5], new NumberTermImpl(nextPos.y));
    }
    
}