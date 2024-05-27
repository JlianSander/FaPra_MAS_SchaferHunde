package simulations;

import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.HashMap;
import java.util.Map;

import model.AgentInfo;

public class Simulation {
    private static final Logger logger = Logger.getLogger(Simulation.class.getName());
    private long startTime;
    private Map<String, String> sheepCapturedTimes = new HashMap<>();

    public void start() {
        startTime = System.currentTimeMillis();
        logger.info("--- Simulation started ---");
    }

    public void end() {
        long endTime = System.currentTimeMillis();
        long duration = endTime - startTime;

        String formattedDuration = formatDuration(duration);
        logger.info("--- Simulation ended ---");
        logger.info("Simulation duration: " + formattedDuration);

        SimulationFileWriter.writeResults(formattedDuration, sheepCapturedTimes);
    }

    public void sheepCaptured(AgentInfo sheep) {
        long currentTime = System.currentTimeMillis();
        long elapsed = currentTime - startTime;

        String formattedElapsed = formatDuration(elapsed);
        sheepCapturedTimes.put(sheep.getJasonId(), formattedElapsed);

        logger.info("Sheep " + sheep.getJasonId() + " captured at " + formattedElapsed);
    }

    private String formatDuration(long duration) {
        long minutes = TimeUnit.MILLISECONDS.toMinutes(duration);
        long seconds = TimeUnit.MILLISECONDS.toSeconds(duration) - TimeUnit.MINUTES.toSeconds(minutes);
        long milliseconds = duration - TimeUnit.MINUTES.toMillis(minutes) - TimeUnit.SECONDS.toMillis(seconds);

        return String.format("%02d:%02d:%d", minutes, seconds, milliseconds);
    }
}