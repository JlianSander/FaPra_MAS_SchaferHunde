package jia;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.StringTermImpl;
import jason.asSyntax.Term;
import jason.stdlib.kill_agent;
import model.AgentInfo;
import service.AgentDB;

public class kill_and_decommission_agent extends kill_agent {
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        String agentName = ts.getAgArch().getAgName();
        GridModel model = GridModel.getInstance();
        AgentDB agentDB = model.getAgentDB();
        AgentInfo agent = agentDB.getAgentByJasonId(agentName);

        GridModel.getInstance().removeAgent(agent);
        agentDB.removeAgent(agent.getCartagoId());

        return super.execute(ts, un, new Term[] { new StringTermImpl(agentName) });
    }
}
