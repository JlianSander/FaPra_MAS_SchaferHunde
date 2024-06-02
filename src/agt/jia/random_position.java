package jia;

import java.util.Random;
import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class random_position extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        Random rand = new Random();
        int maxX = GridModel.getInstance().getWidth() - 1;
        int maxY = GridModel.getInstance().getHeight() - 1;

        int x = rand.nextInt(maxX + 1); // Include maxX
        int y = rand.nextInt(maxY + 1); // Include maxY

        Location loc = new Location(x, y);
        while (!GridModel.getInstance().isFree(loc)) {
            x = rand.nextInt(maxX + 1);
            y = rand.nextInt(maxY + 1);
            loc = new Location(x, y);
        }

        return un.unifies(args[0], new NumberTermImpl(loc.x)) && un.unifies(args[1], new NumberTermImpl(loc.y));
    }
}
