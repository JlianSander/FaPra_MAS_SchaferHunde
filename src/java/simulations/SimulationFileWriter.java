package simulations;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

public class SimulationFileWriter {
    public static void writeResults(String duration, Map<String, String> sheepCapturedTimes) {
        String jcm = System.getProperty("simName");
        if (jcm.indexOf('.') == -1) {
            jcm += ".jcm";
        }
        String simName = jcm.substring(0, jcm.lastIndexOf('.'));

        String prefixDir = "simulations/results";
        String fullDir = String.format("%s/%s", prefixDir, simName);
        if (!new File(fullDir).exists()) {
            new File(fullDir).mkdir();
        }

        int count = 1;
        File dir = new File(fullDir);
        File[] files = dir.listFiles();
        if (files != null) {
            for (File file : files) {
                if (file.getName().endsWith(".txt")) {
                    count++;
                }
            }
        }

        String filePath = String.format("%s/%s_%d.txt", fullDir, simName, count);

        System.out.println("Writing results to " + filePath);
        try (FileWriter writer = new FileWriter(filePath)) {
            writeLine(writer, "Simulation: " + jcm);
            writeLine(writer, "Duration: " + duration);
            writeLine(writer, "Sheeps: {");
            for (Map.Entry<String, String> entry : sheepCapturedTimes.entrySet()) {
                writeLine(writer, entry.getKey() + ": " + entry.getValue());
            }
            writeLine(writer, "}");
            System.out.println("Results written to " + filePath);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void writeLine(FileWriter writer, String line) throws IOException {
        writer.write(line + "\n");
    }
}