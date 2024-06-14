package jia;

import java.util.Iterator;

import jason.NoValueException;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.Atom;
import jason.asSyntax.Literal;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.StringTerm;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

import grid.GridModel;
import util.PropertiesLoader;
import model.AgentInfo;
import service.AgentDB;
import jia.util.AgentUtil;
import jia.util.BeliefBaseManager;

public class look_around extends DefaultInternalAction{
    
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        
        AgentInfo agent = AgentUtil.getAgentInfoFromTs(ts);
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);
        GridModel model = GridModel.getInstance();
        AgentDB agentDB = AgentDB.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer range = loader.getProperty("vision_range", Integer.class);
        
        for(int i = -range; i < range + 1; i++){
            for(int j = -range; j < range + 1; j++){
                if(i == 0 && j == 0) continue;  //don't obeserve own position

                Location loc = new Location(ownLoc.x + i, ownLoc.y + j);
                AgentInfo seenAgent = agentDB.getAgentByLocation(loc.x, loc.y);
                if(seenAgent == null) continue; //no agent found on the location

                //remove all beliefs about this agent's old position or other agents at this position
                var beliefs = BeliefBaseManager.getBeliefs(ts, "pos_agent", 3);
                if(beliefs != null){
                    while(beliefs.hasNext()) {
                        Literal belief = beliefs.next();
                        try {
                            int posX = (int)((NumberTerm)belief.getTerm(0)).solve();
                            int posY = (int)((NumberTerm)belief.getTerm(1)).solve();
                            String agentID = ((Atom)belief.getTerm(2)).toString();
                            if (agentID.equals(seenAgent.getJasonId()) || posX == loc.x && posY == loc.y) {
                                //other belief about same agent or belief about some other agent, who was thouhgt to be on this position
                                BeliefBaseManager.removeBelief(ts, belief);
                            }
                        } catch (NoValueException e) {
                            ts.getAg().getLogger().info("ERROR in 'look_around' noValueException : " + e.getMessage());
                        }
                    }
                }
                
                ts.getAg().getLogger().info("add position of agent " + seenAgent.getJasonId());                                                     //DEBUG
                BeliefBaseManager.addBelief(ts, "pos_agent", null, loc.x, loc.y, new Atom(seenAgent.getJasonId()));
            }
        }

        return true;
    }
}
