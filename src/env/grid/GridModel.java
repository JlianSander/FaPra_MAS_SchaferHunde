package grid;

import grid.util.GridModelFileParser;
import grid.util.GridProcessor;
import jason.environment.grid.GridWorldModel;
import jason.environment.grid.Location;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class GridModel extends GridWorldModel {
    static final int CORRAL = 16;
    static final int SHEEP = 32;

    private static GridProcessor gridProcessor;
    private static GridModel model = null;

    // Private constructor for singleton
    private GridModel(int width, int height) {
        super(width, height, 1000);
        commonInit(width, height);
    }

    public static GridModel getInstance() {
        if (model == null)
            throw new IllegalStateException("GridModel not initialized");

        return model;
    }

    // Factory method to create the singleton instance from parameters
    public static synchronized GridModel create(int size, int corralWidth, int corralHeight) {
        model = new GridModel(size, size);

        // Define corral
        int startX = 1;
        int startY = 1;
        for (int i = startX; i < startX + corralWidth; i++) {
            for (int j = startY; j < startY + corralHeight; j++) {
                model.add(CORRAL, i, j);
            }
        }

        // Initialize the grid with obstructions
        float obstacleDensity = 0.2f;
        gridProcessor.processEntireGrid(loc -> model.isFree(CORRAL, loc)
                && Math.random() < obstacleDensity,
                loc -> model.add(OBSTACLE, loc),
                c -> false);
        return getInstance();
    }

    // Factory method to create the singleton instance from file
    public static synchronized GridModel create(String filePath) {
        char[][] gridData = GridModelFileParser.parseGridFile(filePath);
        int width = gridData[0].length;
        int height = gridData.length;
        model = new GridModel(width, height);
        model.loadFromFile(gridData);
        return getInstance();
    }

    private static void commonInit(int width, int height) {
        gridProcessor = new GridProcessor(width, height);
    }

    private void loadFromFile(char[][] gridData) {
        this.width = gridData[0].length;
        this.height = gridData.length;
        this.data = new int[width][height];

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                switch (gridData[y][x]) {
                    case 'O':
                        // Regular cell, do nothing
                        break;
                    case 'X':
                        add(OBSTACLE, x, y);
                        break;
                    case 'C':
                        add(CORRAL, x, y);
                        break;
                }
            }
        }
    }

    public Location getFreePos() {
        return super.getFreePos();
    }

    public List<Location> getNeighborhood(Location loc, int range, Predicate<Location> filter) {
        List<Location> neighbors = new ArrayList<>();
        for (int dx = -range; dx <= range; dx++) {
            for (int dy = -range; dy <= range; dy++) {
                int newX = loc.x + dx;
                int newY = loc.y + dy;
                Location newLoc = new Location(newX, newY);
                if (filter.test(newLoc)) {
                    neighbors.add(newLoc);
                }
            }
        }
        return neighbors;
    }
}