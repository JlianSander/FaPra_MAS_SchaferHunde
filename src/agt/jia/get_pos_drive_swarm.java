package jia;

import jia.util.DriveStrategy_1;
import jia.util.SwarmManipulator;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class get_pos_drive_swarm extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int centerX = (int) ((NumberTerm) args[0]).solve();
        int centerY = (int) ((NumberTerm) args[1]).solve();
        int radius = (int) ((NumberTerm) args[2]).solve();
        int positionNumber = (int) ((NumberTerm) args[3]).solve();
        var swarmCenter = new Location(centerX, centerY);
        var swarm = new SwarmManipulator(swarmCenter, radius);

        

        var get_corral = new get_corral_area();
        get_corral.execute(ts, un, args);
        var corral = get_corral.getCorral();

        var agentLoc = DriveStrategy_1.positionAgent(ts, swarm, corral, positionNumber);

        return un.unifies(args[4], new NumberTermImpl(agentLoc.x))
                && un.unifies(args[5], new NumberTermImpl(agentLoc.y));
    }
}
