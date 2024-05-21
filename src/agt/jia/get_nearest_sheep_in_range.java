package jia;

import jason.asSemantics.*;
import jason.asSyntax.*;
import java.util.*;
import jason.bb.BeliefBase;

public class get_nearest_sheep_in_range extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        // Get agent's current position and search range
        int agX = (int)((NumberTerm)args[0]).solve();
        int agY = (int)((NumberTerm)args[1]).solve();
        int range = (int)((NumberTerm)args[4]).solve();

        // Initialize variables for nearest flock
        int nearestX = -1;
        int nearestY = -1;
        double nearestDistance = Double.MAX_VALUE;

        // Get the agent's belief base
        BeliefBase beliefBase = ts.getAg().getBB();

        // Iterate over all beliefs in the belief base
        for (Iterator<Literal> iterator = beliefBase.iterator(); iterator.hasNext();) {
            Literal belief = iterator.next();
            // Check if the belief has the desired predicate
            if (belief.getFunctor().equals("sheep_flock") && belief.getArity() == 3) {
                Term flockX = belief.getTerm(0);
                Term flockY = belief.getTerm(1);
                Term flockSizeTerm = belief.getTerm(2);

                if (flockX.isNumeric() && flockY.isNumeric() && flockSizeTerm.isNumeric()) {
                    int flockPosX = (int)((NumberTerm)flockX).solve();
                    int flockPosY = (int)((NumberTerm)flockY).solve();
                    int flockSize = (int)((NumberTerm)flockSizeTerm).solve();

                    double distance = Math.sqrt(Math.pow(agX - flockPosX, 2) + Math.pow(agY - flockPosY, 2));

                    if (flockSize > 7 && distance <= range && distance < nearestDistance) {
                        nearestX = flockPosX;
                        nearestY = flockPosY;
                        nearestDistance = distance;
                    }
                }
            }
        }

        // Return nearest flock coordinates
        return un.unifies(args[2], new NumberTermImpl(nearestX)) && 
               un.unifies(args[3], new NumberTermImpl(nearestY));
    }
}
