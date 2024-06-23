package jia;

import jason.NoValueException;
import jason.asSemantics.*;
import jason.asSyntax.*;
import jason.bb.BeliefBase;
import jason.environment.grid.Location;
import jia.util.AgentUtil;

public class check_nearby_sheep extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);

        Location nearestSheep = new Location(-1, -1);

        // Iterate over all beliefs to find individual sheep
        for (Literal belief : ts.getAg().getBB()) {
            if (belief.getFunctor().equals("sheep")) {
                String sheepID = belief.getTerm(0).toString();
                int[] sheepPos = getSheepPosition(ts, sheepID);
                if (sheepPos == null) {
                    continue;
                }
                int sheepX = sheepPos[0];
                int sheepY = sheepPos[1];

                Location sheepLoc = new Location(sheepX, sheepY);
                double distance = ownLoc.distanceChebyshev(sheepLoc);

                Boolean los = (Boolean) new in_line_of_sight().execute(ts, un,
                        new Term[] { new NumberTermImpl(ownLoc.x),
                                new NumberTermImpl(ownLoc.y), new NumberTermImpl(sheepLoc.x),
                                new NumberTermImpl(sheepLoc.y) });

                if (!los)
                    continue;

                if (distance < ownLoc.distanceChebyshev(nearestSheep)) {
                    nearestSheep = sheepLoc;
                }
            }
        }

        return un.unifies(args[0], new NumberTermImpl(nearestSheep.x))
                && un.unifies(args[1], new NumberTermImpl(nearestSheep.y));
    }

    private int[] getSheepPosition(TransitionSystem ts, String sheepID) {
        BeliefBase beliefBase = ts.getAg().getBB();
        for (Literal belief : beliefBase) {
            if (belief.getFunctor().equals("pos_agent")) {
                try {
                    String agentID = ((Atom) belief.getTerm(2)).toString();
                    if (agentID.equals(sheepID)) {
                        int posX = (int) ((NumberTerm) belief.getTerm(0)).solve();
                        int posY = (int) ((NumberTerm) belief.getTerm(1)).solve();
                        return new int[] { posX, posY };
                    }
                } catch (NoValueException e) {
                    throw new RuntimeException("Error while getting agent ID from belief");
                }
            }
        }
        return null;
    }
}