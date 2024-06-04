package jia.util;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.LiteralImpl;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.bb.BeliefBase;
import util.PropertiesLoader;

public class init_agent extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) {
        PropertiesLoader loader = PropertiesLoader.getInstance();

        int waitTime = loader.getProperty("sheep_wait_duration", Integer.class);

        switch (AgentUtil.getAgentInfoFromTs(ts).getAgentType()) {
            case GridModel.SHEEP:
                break;

            case GridModel.HOUND:
                waitTime = (int) (waitTime * loader.getProperty("hound_wait_ratio", Double.class));

                addBelief(ts, "limit_distance_assumption_hound_driving",
                        loader.getProperty("hound_limit_distance_assumption_hound_driving", Integer.class));

                addBelief(ts, "limit_number_agents_driving_swarm",
                        loader.getProperty("hound_limit_number_agents_driving_swarm", Integer.class));

                addBelief(ts, "limit_radius_swarm",
                        loader.getProperty("hound_limit_radius_swarm", Integer.class));
                break;

            default:
                throw new RuntimeException("Invalid agent type");
        }

        addBelief(ts, "waitTime", waitTime);

        return true;
    }

    private void addBelief(TransitionSystem ts, String name, Object... terms) {
        try {
            BeliefBase bb = ts.getAg().getBB();
            LiteralImpl literal = new LiteralImpl(name);

            for (Object object : terms) {
                if (object instanceof Integer) {
                    literal.addTerm(new NumberTermImpl((Integer) object));
                } else if (object instanceof Double) {
                    literal.addTerm(new NumberTermImpl((Double) object));
                } else {
                    throw new Exception("Invalid object type");
                }
            }

            bb.add(literal);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}