package simulations;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.ArrayList;

import org.javatuples.Pair;

public class SimulationFileWriter {
    private static FileWriter writer;

    public static void writeResults(String duration, List<Pair<String, String>> sheepCapturedTimes, int sheepCount) {
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
        writeCsvFile(jcm, simName, fullDir, duration, sheepCapturedTimes, sheepCount);
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
        } catch (IOException e) {
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

    private static void writeCsvFile(String jcm, String simName, String fullDir, String duration,
            List<Pair<String, String>> sheepCapturedTimes, int sheepCount) {
        String filePath = String.format("%s/%s.csv", fullDir, simName);
        File csvFile = new File(filePath);
        boolean fileExists = csvFile.exists();

        List<String> lines = new ArrayList<>();

        if (fileExists) {
            try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
                String line;
                while ((line = br.readLine()) != null) {
                    lines.add(line);
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        } else {
            lines.add("Simulation:, " + jcm);
            lines.add("Duration:, " + duration);
            lines.add("");
            lines.add("");
            lines.add("");
        }

        int headerEndIndex = lines.size();
        for (int i = 0; i < lines.size(); i++) {
            if (lines.get(i).startsWith("Sheep count")) {
                headerEndIndex = i;
                break;
            }
        }

        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        String currentTime = dtf.format(now);

        if (headerEndIndex < lines.size()) {
            lines.set(headerEndIndex, lines.get(headerEndIndex) + ", " + currentTime);
        } else {
            lines.add("Sheep count, " + currentTime);
        }

        for (int i = 0; i < sheepCount; i++) {
            String capturedTime = i < sheepCapturedTimes.size() ? sheepCapturedTimes.get(i).getValue1() : "-";
            if (headerEndIndex + 1 + i < lines.size()) {
                lines.set(headerEndIndex + 1 + i, lines.get(headerEndIndex + 1 + i) + ", " + capturedTime);
            } else {
                lines.add(i + ", " + capturedTime);
            }
        }

        try {
            writer = new FileWriter(filePath);
            for (String line : lines) {
                writer.write(line + "\n");
            }
            System.out.println("Results appended to " + filePath);
        } catch (IOException e) {
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