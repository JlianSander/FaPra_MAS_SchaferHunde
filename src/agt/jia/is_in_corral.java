package jia;

import java.util.NoSuchElementException;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class is_in_corral extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {

        try {
            int locX = (int) ((NumberTerm) args[0]).solve();
            int locY = (int) ((NumberTerm) args[1]).solve();
            var loc = new Location(locX, locY);

            var get_corral = new get_corral_area();
            get_corral.init();
            var corral = get_corral.corral();

            boolean result = corral.contains(loc);

            ts.getLogger().info("Result of jia 'is_in_corral' for (" + locX + ", " + locY + "): " + result);

            return result;
        } catch (NoSuchElementException e) {
            return false;
        }
    }
}
