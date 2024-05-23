package jia;

import jason.asSemantics.*;
import jason.asSyntax.*;
import java.util.Random;

public class random_position extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        Random rand = new Random();
        int maxX = 20; // Beispiel maximale X-Koordinate
        int maxY = 20; // Beispiel maximale Y-Koordinate

        int x = rand.nextInt(maxX);
        int y = rand.nextInt(maxY);

        return un.unifies(args[0], new NumberTermImpl(x)) && un.unifies(args[1], new NumberTermImpl(y));
    }
}
