package jia.util;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.StringTermImpl;
import jason.asSyntax.Term;
import jason.stdlib.kill_agent;
import model.AgentInfo;

public class kill_and_decommission_agent extends kill_agent {
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        AgentInfo agent = AgentUtil.getAgentInfoFromTs(ts);

        GridModel.getInstance().removeAgent(agent);

        String agentName = agent.getJasonId();
        return super.execute(ts, un, new Term[] { new StringTermImpl(agentName) });
    }
}
