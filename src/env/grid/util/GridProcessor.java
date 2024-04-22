package grid.util;

import java.util.function.Consumer;
import java.util.function.Predicate;
import jason.environment.grid.Location;

public class GridProcessor {
    private int width, height;

    public GridProcessor(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public void processEntireGrid(Predicate<Location> pred,
            Consumer<Location> func) {
        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
                Location loc = new Location(i, j);
                if (pred.test(loc)) {
                    func.accept(loc);
                }
            }
        }
    }
}