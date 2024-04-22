package grid.util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class GridModelFileParser {
    // Exception to throw when grid dimensions are inconsistent
    public static class InconsistentGridWidthException extends Exception {
        public InconsistentGridWidthException(String message) {
            super(message);
        }
    }

    // Exception to throw when grid is not square
    public static class NonSquareGridException extends Exception {
        public NonSquareGridException(String message) {
            super(message);
        }
    }

    // Reads the grid file and returns the grid content
    public static char[][] parseGridFile(String filepath) {
        try (BufferedReader reader = new BufferedReader(new FileReader(filepath))) {
            StringBuilder firstLine = new StringBuilder(reader.readLine());

            int width = firstLine.length();
            StringBuilder lines = new StringBuilder(firstLine);
            String line;
            int lineCount = 1;

            while ((line = reader.readLine()) != null) {
                if (line.length() != width) {
                    throw new InconsistentGridWidthException(
                            "Could not load grid template file.\nInconsistent line widths at line " + lineCount);
                }
                lines.append(line);
                lineCount++;
            }
            reader.close();

            if (lineCount != width) {
                throw new NonSquareGridException(String.format(
                        "Could not load grid template file.\nGrid is not square. Line count: %d, width: %d", lineCount,
                        width));
            }

            char[][] grid = new char[lineCount][width];
            for (int i = 0; i < lineCount; i++) {
                grid[i] = lines.substring(i * width, (i + 1) * width).toCharArray();
            }
            return grid;
        } catch (IOException | InconsistentGridWidthException | NonSquareGridException e) {
            throw new RuntimeException("Error while parsing grid file", e);
        }
    }
}