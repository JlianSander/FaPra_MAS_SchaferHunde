package jia.hounds;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.hounds.SwarmManipulator;

public class get_evasion_directions extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int centerX = (int) ((NumberTerm) args[0]).solve();
        int centerY = (int) ((NumberTerm) args[1]).solve();
        int radius = (int) ((NumberTerm) args[2]).solve();
        var swarmCenter = new Location(centerX, centerY);
        var swarm = new SwarmManipulator(swarmCenter, radius);

        var get_corral = new get_corral_area();
        get_corral.execute(ts, un, args);
        var corral = get_corral.getCorral();

        var swarmDirection = swarm.getDirectionTo(ts, corral.center());
        var evasionDirection = swarmDirection.mapMultiply(-1);

        return un.unifies(args[3], new NumberTermImpl(Math.round(evasionDirection.getEntry(0))))
                && un.unifies(args[4], new NumberTermImpl(Math.round(evasionDirection.getEntry(1))));
    }
}
