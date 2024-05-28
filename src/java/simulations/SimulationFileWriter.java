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

import model.ScenarioInfo;

public class SimulationFileWriter {
    private static FileWriter writer;

    public static void writeResults(ScenarioInfo scenarioInfo, List<Pair<String, String>> sheepCapturedTimes,
            String duration) {
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

        writeTxtFile(jcm, simName, fullDir, duration, scenarioInfo, sheepCapturedTimes);
        writeCsvFile(jcm, simName, fullDir, duration, scenarioInfo, sheepCapturedTimes);
    }

    private static void writeTxtFile(String jcm, String simName, String fullDir, String duration,
            ScenarioInfo scenarioInfo, List<Pair<String, String>> sheepCapturedTimes) {
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
            writeLine("Sheep amount: " + scenarioInfo.getTotalSheepCount());
            writeLine("Hound amount: " + scenarioInfo.getTotalHoundCount());
            writeLine("Sheep wait time: " + scenarioInfo.getSheepWaitTime());
            writeLine("Hound wait time: " + scenarioInfo.getHoundWaitTime());
            writeLine("Hound wait ratio: " + scenarioInfo.getHoundRelativeWaitTime());
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
            ScenarioInfo scenarioInfo, List<Pair<String, String>> sheepCapturedTimes) {
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
            lines.add("Szenario:, " + jcm);
            lines.add("Schafe:, " + scenarioInfo.getTotalSheepCount());
            lines.add("Hunde:, " + scenarioInfo.getTotalHoundCount());
            lines.add("Schafe Timeout:, " + scenarioInfo.getSheepWaitTime());
            lines.add("Hunde Timeout:, " + scenarioInfo.getHoundWaitTime());
            lines.add("Hunde Timeout relativ:, " + scenarioInfo.getHoundRelativeWaitTime());
            lines.add("");
            lines.add("");
            lines.add("");
            lines.add("Zeitpunkt:");
            lines.add("Laufzeit:");
        }

        int headerEndIndex = lines.size();
        for (int i = 0; i < lines.size(); i++) {
            if (lines.get(i).startsWith("Anzahl Schafe")) {
                headerEndIndex = i;
                break;
            }
        }

        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("dd-MM HH:mm:ss");
        String currentTime = dtf.format(now);

        lines.set(9, lines.get(9) + ", " + currentTime);
        lines.set(10, lines.get(10) + ", " + duration);

        if (headerEndIndex < lines.size()) {
            lines.set(headerEndIndex, "Anzahl Schafe,");
        } else {
            lines.add("Anzahl Schafe,");
        }

        for (int i = 0; i < scenarioInfo.getTotalSheepCount(); i++) {
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