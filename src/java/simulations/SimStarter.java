package simulations;

import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;
import jacamo.infra.JaCaMoLauncher;
import jason.JasonException;
import model.HoundDriveStrategyConfig;
import util.FileLister;
import util.PropertiesLoader;

public class SimStarter {
    private static final Logger logger = Logger.getLogger(SimStarter.class.getName());

    private static Process simulationProcess;

    public static void endSimulation() {
        if (simulationProcess != null && simulationProcess.isAlive()) {
            try {
                simulationProcess.destroy();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) throws JasonException, IOException {
        String dir = "simulations/scenarios";
        List<String> scenarios = FileLister.getFileNames(dir, false);

        PropertiesLoader loader = PropertiesLoader.getInstance();

        int houndStrategyDriveClusterSwarm = loader.getProperty("hound_strategy_cluster_swarm_amount", Integer.class);
        int houndStrategyDriveSelectSwarm = loader.getProperty("hound_strategy_select_swarm_amount", Integer.class);
        int houndStrategyDriveDrive = loader.getProperty("hound_strategy_drive_amount", Integer.class);

        for (int i = 1; i <= houndStrategyDriveClusterSwarm; i++) {
            for (int j = 1; j <= houndStrategyDriveSelectSwarm; j++) {
                for (int k = 1; k <= houndStrategyDriveDrive; k++) {
                    logger.info("+++ Starting simulation +++");
                    logger.info("Hound DRIVE strategy:");
                    logger.info("ClusterSwarm: " + i);
                    logger.info("SelectSwarm: " + j);
                    logger.info("Drive: " + k);

                    for (String scenario : scenarios) {

                        // Spawn the simulation in a separate JVM process
                        ProcessBuilder processBuilder = new ProcessBuilder(
                                "java",
                                "-Xms16g",
                                "-Xmx16g",
                                "-XX:+UseG1GC",
                                "-XX:MaxGCPauseMillis=200",
                                "-DsimName=" + scenario,
                                "-DhoundStrategyClusterSwarm=" + i,
                                "-DhoundStrategySelectSwarm=" + j,
                                "-DhoundStrategyDrive=" + k,
                                "-cp",
                                System.getProperty("java.class.path"),
                                JaCaMoLauncher.class.getName(),
                                dir + "/" + scenario);

                        try {
                            simulationProcess = processBuilder.start();

                            // Wait for the sim to complete
                            simulationProcess.waitFor();
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }
}