package util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class PredefinedStrategyLoader {
    public static List<String> parseStrategies() {
        String filepath = "simulations/predefined-strategies.txt";
        try (BufferedReader reader = new BufferedReader(new FileReader(filepath))) {
            List<String> strategies = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                if (strategies.contains(line)) {
                    throw new RuntimeException("Error while parsing strategy file: Duplicate strategy found");
                }
                if (line.length() != 4) {
                    throw new RuntimeException("Error while parsing strategy file: Invalid strategy length");
                }
                for (char c : line.toCharArray()) {
                    if (!Character.isDigit(c)) {
                        throw new RuntimeException("Error while parsing strategy file: Invalid character in strategy");
                    }
                }
                strategies.add(line);
            }
            return strategies;
        } catch (IOException e) {
            throw new RuntimeException("Error while parsing strategy file", e);
        }
    }
}