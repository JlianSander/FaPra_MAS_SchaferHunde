package model;

import util.PropertiesLoader;

public class HoundDriveStrategyConfig {
    private static int clusterSwarm = Integer.MIN_VALUE;
    private static int selectSwarm = Integer.MIN_VALUE;
    private static int drive = Integer.MIN_VALUE;

    public static int getClusterSwarm() {
        if (clusterSwarm == Integer.MIN_VALUE) {
            if (System.getProperty("houndStrategyClusterSwarm") != null) {
                clusterSwarm = Integer.parseInt(System.getProperty("houndStrategyClusterSwarm"));
            } else {
                clusterSwarm = PropertiesLoader.getInstance().getProperty("hound_strategy_cluster_swarm",
                        Integer.class);
            }
        }

        return clusterSwarm;
    }

    public static int getSelectSwarm() {
        if (selectSwarm == Integer.MIN_VALUE) {
            if (System.getProperty("houndStrategySelectSwarm") != null) {
                selectSwarm = Integer.parseInt(System.getProperty("houndStrategySelectSwarm"));
            } else {
                selectSwarm = PropertiesLoader.getInstance().getProperty("hound_strategy_select_swarm", Integer.class);
            }
        }

        return selectSwarm;
    }

    public static int getDrive() {
        if (drive == Integer.MIN_VALUE) {
            if (System.getProperty("houndStrategyDrive") != null) {
                drive = Integer.parseInt(System.getProperty("houndStrategyDrive"));
            } else {
                drive = PropertiesLoader.getInstance().getProperty("hound_strategy_drive", Integer.class);
            }
        }

        return drive;
    }
}
