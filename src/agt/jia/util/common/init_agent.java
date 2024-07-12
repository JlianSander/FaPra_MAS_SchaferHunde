package jia.util.common;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.Term;
import model.HoundDriveStrategyConfig;
import model.HoundSearchStrategyConfig;
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

                                BeliefBaseManager.addBelief(ts, "search_strategy", null,
                                                HoundSearchStrategyConfig.getStrategy());

                                BeliefBaseManager.addBelief(ts, "strategy_cluster_swarm", null,
                                                HoundDriveStrategyConfig.getClusterSwarm());

                                BeliefBaseManager.addBelief(ts, "strategy_select_swarm", null,
                                                HoundDriveStrategyConfig.getSelectSwarm());

                                BeliefBaseManager.addBelief(ts, "strategy_drive", null,
                                                HoundDriveStrategyConfig.getDrive());

                                BeliefBaseManager.addBelief(ts, "cluster_swarm_limit_distance_member", null,
                                                loader.getProperty("hound_cluster_swarm_limit_distance_member",
                                                                Integer.class));

                                BeliefBaseManager.addBelief(ts, "limit_distance_assumption_hound_driving", null,
                                                loader.getProperty("hound_limit_distance_assumption_hound_driving",
                                                                Integer.class));

                                BeliefBaseManager.addBelief(ts, "limit_number_agents_driving_swarm", null,
                                                loader.getProperty("hound_limit_number_agents_driving_swarm",
                                                                Integer.class));

                                BeliefBaseManager.addBelief(ts, "search_jammed_retries", null,
                                                loader.getProperty("hound_search_jammed_retries", Integer.class));

                                BeliefBaseManager.addBelief(ts, "search_space_between_lines", null,
                                                loader.getProperty("hound_search_space_between_lines", Integer.class));

                                BeliefBaseManager.addBelief(ts, "search_wait_jammed", null,
                                                loader.getProperty("hound_search_wait_jammed", Integer.class));

                                BeliefBaseManager.addBelief(ts, "select_swarm_weight_proximity", null,
                                                loader.getProperty("hound_select_swarm_weight_proximity",
                                                                Integer.class));

                                BeliefBaseManager.addBelief(ts, "select_swarm_weight_size", null,
                                                loader.getProperty("hound_select_swarm_weight_size", Integer.class));

                                BeliefBaseManager.addBelief(ts, "stay_on_same_position", null,
                                        loader.getProperty("hound_stay_on_same_position", Integer.class));

                                BeliefBaseManager.addBelief(ts, "wait_between_driving", null,
                                                loader.getProperty("hound_wait_between_driving", Integer.class));

                                BeliefBaseManager.addBelief(ts, "wait_ignore_sheep_forget", null,
                                                loader.getProperty("hound_wait_ignore_sheep_forget", Integer.class));

                                BeliefBaseManager.addBelief(ts, "wait_perception", null,
                                                loader.getProperty("hound_wait_perception", Integer.class));
                                break;

                        default:
                                throw new RuntimeException("Invalid agent type");
                }

                BeliefBaseManager.addBelief(ts, "waitTime", null, waitTime);

                return true;
        }
}
