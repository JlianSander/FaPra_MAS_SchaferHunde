package jia.util.common;

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
        AgentInfo agent = AgentUtil.getAgentInfoFromTs(ts);
        String agentName = agent.getJasonId();

        GridModel.getInstance().removeAgent(agent);
        AgentDB.getInstance().removeAgent(agent.getCartagoId());

        return super.execute(ts, un, new Term[] { new StringTermImpl(agentName) });
    }
}
