package util;

import java.io.IOException;
import java.nio.file.*;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class FileLister {
    public static List<String> getFileNames(String directoryPath, boolean includePath) throws IOException {
        try (Stream<Path> paths = Files.list(Paths.get(directoryPath))) {
            return paths
                    .filter(Files::isRegularFile)
                    .map(path -> includePath ? path.toString() : path.getFileName().toString())
                    .collect(Collectors.toList());
        }
    }
}