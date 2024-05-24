package simulations;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

public class SimulationFileWriter {

    public static void writeResults(String duration, Map<String, String> sheepCapturedTimes) {
        String filePath = "simulations/results/X.txt";

        try (FileWriter writer = new FileWriter(filePath)) {
            writer.write("Duration: " + duration + "\n");
            writer.write("Sheeps: {\n");
            for (Map.Entry<String, String> entry : sheepCapturedTimes.entrySet()) {
                writer.write(entry.getKey() + ": " + entry.getValue() + "\n");
            }
            writer.write("}\n");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}