package jia.hounds;

import java.util.Random;
import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class get_random_position extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        Random rand = new Random();
        int maxX = GridModel.getInstance().getWidth();
        int maxY = GridModel.getInstance().getHeight();

        while (true) {
            Location loc = new Location(rand.nextInt(maxX), rand.nextInt(maxY));
            if (GridModel.getInstance().isFree(loc)) {
                return un.unifies(args[0], new NumberTermImpl(loc.x)) && un.unifies(args[1], new NumberTermImpl(loc.y));
            }
        }
    }
}