package grid;

import grid.util.GridModelFileParser;
import grid.util.GridProcessor;
import jason.environment.grid.GridWorldModel;
import jason.environment.grid.Location;

public class GridModel extends GridWorldModel {
    static final int CORRAL = 16;
    static final int SHEEP = 32;

    private GridProcessor gridProcessor;

    // Random from parameters
    public GridModel(int size, int corralWidth, int corralHeight) {
        super(size, size, 1);
        commonInit();

        // Define corral
        int startX = 1;
        int startY = 1;
        for (int i = startX; i < startX + corralWidth; i++) {
            for (int j = startY; j < startY + corralHeight; j++) {
                add(CORRAL, i, j);
            }
        }

        // Initialize the grid with obstructions
        double obstacleDensity = 0.2;
        gridProcessor.processEntireGrid(loc -> isFree(CORRAL, loc)
                && Math.random() < obstacleDensity,
                loc -> add(OBSTACLE, loc));
    }

    // From file
    public GridModel(String filePath) {
        super(0, 0, 1000);
        commonInit();
        loadFromFile(filePath);
    }

    private void commonInit() {
        gridProcessor = new GridProcessor(width, height);
    }

    private void loadFromFile(String filepath) {
        char[][] gridData = GridModelFileParser.parseGridFile(filepath);
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
}