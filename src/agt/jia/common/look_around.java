package jia.common;

import java.util.HashSet;
import java.util.function.Predicate;

import jason.NoValueException;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.Atom;
import jason.asSyntax.Literal;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.common.AgentUtil;
import jia.util.common.BeliefBaseManager;
import util.PropertiesLoader;
import model.AgentInfo;
import service.AgentDB;

public class look_around extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);
        AgentDB agentDB = AgentDB.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer range = loader.getProperty("vision_range", Integer.class);

        for (int i = -range; i < range + 1; i++) {
            for (int j = -range; j < range + 1; j++) {
                Location loc = new Location(ownLoc.x + i, ownLoc.y + j);
                
                if (i == 0 && j == 0){
                    //own position
                    updateLocations(ts, loc, (posX, posY, agentID) -> {
                        return posX == loc.x && posY == loc.y;
                    });
                    continue;
                }
                
                //check if the location is in line of sight
                Boolean los = (Boolean) new in_line_of_sight().execute(ts, un,
                        new Term[] { new NumberTermImpl(ownLoc.x),
                                new NumberTermImpl(ownLoc.y), new NumberTermImpl(loc.x), new NumberTermImpl(loc.y) });

                if (!los)
                    continue;

                AgentInfo seenAgent = agentDB.getAgentByLocation(loc.x, loc.y);
                if (seenAgent == null) {
                    updateLocations(ts, loc, (posX, posY, agentID) -> {
                        return posX == loc.x && posY == loc.y;
                    });
                } else {
                    updateLocations(ts, loc, (posX, posY, agentID) -> {
                        return agentID.equals(seenAgent.getJasonId()) || posX == loc.x && posY == loc.y;
                    });

                    //ts.getAg().getLogger().info("add position of agent " + seenAgent.getJasonId());                                                     //DEBUG
                    BeliefBaseManager.addBelief(ts, "pos_agent", new Atom("percept"), loc.x, loc.y,
                            new Atom(seenAgent.getJasonId()));
                }
            }
        }

        return true;
    }

    private void updateLocations(TransitionSystem ts, Location loc,Function3<Integer, Integer, String, Boolean> filter){
        //remove all beliefs about this agent's old position or other agents at this position
        var oldBeliefs = removeOutdatedBeliefs(ts, loc, "pos_agent", filter);
        //keep the old beliefs in memory
        for(var belief : oldBeliefs){
            try {
                int posX = (int) ((NumberTerm) belief.getTerm(0)).solve();
                int posY = (int) ((NumberTerm) belief.getTerm(1)).solve();
                String agentID = ((Atom) belief.getTerm(2)).toString();
                //remove the beliefs in memory to be replaced
                removeOutdatedBeliefs(ts, loc, "pos_agent_old", (pX, pY, id) -> {  return id.equals(agentID) || posX == pX && posY == pY; });
                BeliefBaseManager.addBelief(ts, "pos_agent_old", new Atom("percept"), loc.x, loc.y, new Atom(agentID));
            } catch (NoValueException e) {
                ts.getAg().getLogger().info("ERROR in 'look_around' noValueException : " + e.getMessage());
            }
        }
    }


    private HashSet<Literal> removeOutdatedBeliefs(TransitionSystem ts, Location loc, String name,
            Function3<Integer, Integer, String, Boolean> filter) {
        var oldBeliefs = new HashSet<Literal>();
        var beliefs = BeliefBaseManager.getBeliefs(ts, name, 3);
        if (beliefs != null) {
            while (beliefs.hasNext()) {
                Literal belief = beliefs.next();
                try {
                    int posX = (int) ((NumberTerm) belief.getTerm(0)).solve();
                    int posY = (int) ((NumberTerm) belief.getTerm(1)).solve();
                    String agentID = ((Atom) belief.getTerm(2)).toString();
                    if (filter.apply(posX, posY, agentID)) {
                        //other belief about same agent or belief about some other agent, who was thouhgt to be on this position
                        BeliefBaseManager.removeBelief(ts, belief);
                        oldBeliefs.add(belief);
                    }
                } catch (NoValueException e) {
                    ts.getAg().getLogger().info("ERROR in 'look_around' noValueException : " + e.getMessage());
                }
            }
        }
        return oldBeliefs;
    }

    @FunctionalInterface
    private interface Function3<One, Two, Three, Result> {
        public Result apply(One one, Two two, Three three);
    }
}
