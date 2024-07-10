package simulations;

import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.JButton;
import javax.swing.JFrame;
import java.awt.Dimension;
import java.awt.Toolkit;

import jacamo.infra.JaCaMoLauncher;
import jason.JasonException;
import util.FileLister;
import util.PropertiesLoader;

public class SimStarter {
    private static final Logger logger = Logger.getLogger(SimStarter.class.getName());

    private static Process simulationProcess;
    private static JFrame controlFrame;

    private static void createControlWindow() {
        controlFrame = new JFrame("Simulation Control");
        JButton stopButton = new JButton("Stop Simulation");

        stopButton.addActionListener(e -> {
            destroy();
        });

        controlFrame.getContentPane().add(stopButton);
        controlFrame.setSize(200, 100);

        stopButton.setBackground(java.awt.Color.RED);
        stopButton.setForeground(java.awt.Color.WHITE);

        controlFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        int x = screenSize.width - controlFrame.getWidth();
        int y = screenSize.height - controlFrame.getHeight();
        controlFrame.setLocation(x, y);

        controlFrame.setVisible(true);
    }

    public static void endSimulation() {
        if (simulationProcess != null && simulationProcess.isAlive()) {
            try {
                simulationProcess.destroy();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private static void destroy() {
        endSimulation();
        System.exit(0);
    }

    public static void main(String[] args) throws JasonException, IOException {
        createControlWindow();

        int times = 0;
        try {
            times = Integer.parseInt(System.getProperty("times"));
        } catch (NumberFormatException e) {
            logger.severe("Invalid number of times to run the simulation");
            System.exit(1);
        }

        boolean full = Boolean.parseBoolean(System.getProperty("full"));

        String dir = "simulations/scenarios";
        List<String> scenarios = FileLister.getFileNames(dir, false);

        PropertiesLoader loader = PropertiesLoader.getInstance();

        int maxI = loader.getProperty("hound_strategy_cluster_swarm_amount", Integer.class);
        int maxJ = loader.getProperty("hound_strategy_select_swarm_amount", Integer.class);
        int maxK = loader.getProperty("hound_strategy_drive_amount", Integer.class);

        int initialI = loader.getProperty("hound_strategy_cluster_swarm", Integer.class);
        int initialJ = loader.getProperty("hound_strategy_select_swarm", Integer.class);
        int initialK = loader.getProperty("hound_strategy_drive", Integer.class);

        System.out.println("=== Starting simulation a total of " + times + " times ===\n");
        if (full) {
            System.out.println("+++ FULL mode: Iterating through all strategies +++\n\n");
        }

        for (int t = 0; t < times; t++) {
            for (int i = full ? 1 : initialI; i <= (full ? maxI : initialI); i++) {
                for (int j = full ? 1 : initialJ; j <= (full ? maxJ : initialJ); j++) {
                    for (int k = full ? 1 : initialK; k <= (full ? maxK : initialK); k++) {
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
                                Process simulationProcess = processBuilder.start();

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

        destroy();
    }
}