package simulations;

import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.javatuples.Pair;

import java.util.ArrayList;
import java.util.List;

import model.AgentInfo;

public class Simulation {
    private static final Logger logger = Logger.getLogger(Simulation.class.getName());
    private long startTime;
    // private Map<String, String> sheepCapturedTimes = new HashMap<>();
    private List<Pair<String, String>> sheepCapturedTimes = new ArrayList<>();

    public void start() {
        startTime = System.currentTimeMillis();
        logger.info("--- Simulation started ---");
    }

    public void end(boolean finishedInTime) {
        String formattedElapsed = "";
        // Bypassing the time needed to actually end the sim if we caught all sheep
        if (finishedInTime) {
            formattedElapsed = sheepCapturedTimes.get(sheepCapturedTimes.size() - 1).getValue1();
        } else {
            long endTime = System.currentTimeMillis();
            long duration = endTime - startTime;
            formattedElapsed = formatDuration(duration);
        }

        logger.info("--- Simulation ended ---");
        logger.info("Simulation duration: " + formattedElapsed);

        SimulationFileWriter.writeResults(formattedElapsed, sheepCapturedTimes);
    }

    public void sheepCaptured(AgentInfo sheep) {
        long currentTime = System.currentTimeMillis();
        long elapsed = currentTime - startTime;

        String formattedElapsed = formatDuration(elapsed);
        sheepCapturedTimes.add(new Pair<String, String>(sheep.getJasonId(), formattedElapsed));

        logger.info("Sheep " + sheep.getJasonId() + " captured at " + formattedElapsed);
    }

    private String formatDuration(long duration) {
        long minutes = TimeUnit.MILLISECONDS.toMinutes(duration);
        long seconds = TimeUnit.MILLISECONDS.toSeconds(duration) - TimeUnit.MINUTES.toSeconds(minutes);
        long milliseconds = duration - TimeUnit.MINUTES.toMillis(minutes) - TimeUnit.SECONDS.toMillis(seconds);

        return String.format("%02d:%02d:%d", minutes, seconds, milliseconds);
    }
}