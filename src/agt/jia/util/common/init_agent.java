package jia.util.common;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.Term;
import jason.runtime.RuntimeServicesFactory;
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
                                double wait_ratio = 0;
                                try{
                                        wait_ratio = getWait_ratio();
                                }catch (java.rmi.RemoteException e) {
                                        ts.getLogger().warning("init_agent::execute ERROR");
                                        e.printStackTrace();
                                }
                                waitTime = (int) Math.round(waitTime * wait_ratio);

                                if(loader.getProperty("hound_no_driving", Boolean.class)){
                                        BeliefBaseManager.addBelief(ts, "no_driving", null);
                                }
                                
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
                                
                                BeliefBaseManager.addBelief(ts, "keep_distance_to_sheep", null,
                                                loader.getProperty("hound_keep_distance_to_sheep",
                                                                Integer.class));

                                BeliefBaseManager.addBelief(ts, "limit_number_agents_driving_swarm", null,
                                                loader.getProperty("hound_limit_number_agents_driving_swarm",
                                                                Integer.class));

                                BeliefBaseManager.addBelief(ts, "offset_to_drive_pos_for_assuming", null,
                                                loader.getProperty("hound_offset_to_drive_pos_for_assuming", Integer.class));

                                BeliefBaseManager.addBelief(ts, "search_jammed_retries", null,
                                                loader.getProperty("hound_search_jammed_retries", Integer.class));

                                BeliefBaseManager.addBelief(ts, "search_space_between_lines", null,
                                                loader.getProperty("hound_search_space_between_lines", Integer.class));

                                BeliefBaseManager.addBelief(ts, "select_swarm_weight_proximity", null,
                                                loader.getProperty("hound_select_swarm_weight_proximity",
                                                                Integer.class));

                                BeliefBaseManager.addBelief(ts, "select_swarm_weight_size", null,
                                                loader.getProperty("hound_select_swarm_weight_size", Integer.class));

                                BeliefBaseManager.addBelief(ts, "stay_on_same_position", null,
                                        loader.getProperty("hound_stay_on_same_position", Integer.class));
                                
                                int waitTimeBetweenDriving = (int) (waitTime * loader.getProperty("hound_wait_ratio_between_driving", Double.class));
                                BeliefBaseManager.addBelief(ts, "wait_between_driving", null, waitTimeBetweenDriving);

                                int waitTimeIgnoreSheep = (int) (waitTime * loader.getProperty("hound_wait_ratio_ignore_sheep_forget", Double.class));
                                BeliefBaseManager.addBelief(ts, "wait_ignore_sheep_forget", null, waitTimeIgnoreSheep);

                                int waitTimePerception = (int) (waitTime * loader.getProperty("hound_wait_ratio_perception", Double.class));
                                BeliefBaseManager.addBelief(ts, "wait_perception", null, waitTimePerception);
                                break;

                        default:
                                throw new RuntimeException("Invalid agent type");
                }

                BeliefBaseManager.addBelief(ts, "waitTime", null, waitTime);

                return true;
        }

        public static double getWait_ratio() throws java.rmi.RemoteException {
                PropertiesLoader loader = PropertiesLoader.getInstance();
                int nbSheep = 0;
                var agts = RuntimeServicesFactory.get().getAgentsNames();
                nbSheep = (int) agts.stream().filter(s -> s.contains("sheep")).count();
                double coeffA = loader.getProperty("hound_wait_ratio_coeff_a", Double.class);
                double coeffB = loader.getProperty("hound_wait_ratio_coeff_b", Double.class);
                double wait_ratio = coeffA * Math.exp(coeffB * nbSheep);
                return wait_ratio;
        }
}
