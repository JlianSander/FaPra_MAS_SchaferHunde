package jia.util;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.Term;
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

                BeliefBaseManager.addBelief(ts, "keep_distance_to_swarm",
                    loader.getProperty("hound_keep_distance_to_swarm", Integer.class));

                BeliefBaseManager.addBelief(ts, "limit_distance_assumption_hound_driving",
                    loader.getProperty("hound_limit_distance_assumption_hound_driving", Integer.class));

                BeliefBaseManager.addBelief(ts, "limit_number_agents_driving_swarm",
                    loader.getProperty("hound_limit_number_agents_driving_swarm", Integer.class));

                BeliefBaseManager.addBelief(ts, "limit_radius_swarm",
                    loader.getProperty("hound_limit_radius_swarm", Integer.class));

                BeliefBaseManager.addBelief(ts, "wait_between_driving",
                    loader.getProperty("hound_wait_between_driving", Integer.class));

                BeliefBaseManager.addBelief(ts, "wait_perception",
                    loader.getProperty("hound_wait_perception", Integer.class));
                break;

            default:
                throw new RuntimeException("Invalid agent type");
        }

        BeliefBaseManager.addBelief(ts, "waitTime", waitTime);

        return true;
    }
}
