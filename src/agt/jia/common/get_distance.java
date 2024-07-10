package jia.common;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class get_distance extends DefaultInternalAction {
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int x1 = (int) ((NumberTerm) args[0]).solve();
        int y1 = (int) ((NumberTerm) args[1]).solve();
        int x2 = (int) ((NumberTerm) args[2]).solve();
        int y2 = (int) ((NumberTerm) args[3]).solve();

        var loc = new Location(x1, y1);
        int distanceChebyshev = new Location(x2, y2).distanceChebyshev(loc);

        //ts.getLogger().info("--------------'get_distance' for (" + x1 + ", " + y1 + ") <-> (" + x2 + "," + y2 + "): " + distanceChebyshev);
        return un.unifies(args[4], new NumberTermImpl(distanceChebyshev));
    }
}