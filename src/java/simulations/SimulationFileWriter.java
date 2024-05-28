package simulations;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import org.javatuples.Pair;

public class SimulationFileWriter {
    private static FileWriter writer;

    public static void writeResults(String duration, List<Pair<String, String>> sheepCapturedTimes) {
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

        writeTxtFile(jcm, simName, fullDir, duration, sheepCapturedTimes);
    }

    private static void writeTxtFile(String jcm, String simName, String fullDir, String duration,
            List<Pair<String, String>> sheepCapturedTimes) {
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
        try {
            writer = new FileWriter(filePath);
            writeLine("Simulation: " + jcm);
            writeLine("Duration: " + duration);
            writeLine("Sheeps: {");
            for (Pair<String, String> pair : sheepCapturedTimes) {
                writeLine(String.format("%s: %s", pair.getValue0(), pair.getValue1()));
            }
            writeLine("}");
            System.out.println("Results written to " + filePath);
        } catch (

        IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (writer != null) {
                    writer.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private static void writeLine(String line) throws IOException {
        writer.write(line + "\n");
    }
}