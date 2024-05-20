package jia;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.Term;
import jason.stdlib.kill_agent;
import model.AgentInfo;
import service.AgentDB;

public class kill_sheep extends kill_agent {
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        GridModel model = GridModel.getInstance();
        AgentDB agentDB = model.getAgentDB();
        AgentInfo agent = agentDB.getAgentByJasonId(args[0].toString());
        GridModel.getInstance().removeAgent(agent);
        agentDB.removeAgent(agent.getCartagoId());

        return super.execute(ts, un, args);
    }
}
