package service;

import java.util.*;

import grid.GridModel;
import jason.environment.grid.Location;

public class GridBFS {
    public static List<Location> gatherLocations(Location start, int amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Amount must be greater than 0");
        }

        List<Location> result = new ArrayList<>();

        GridModel model = GridModel.getInstance();
        int rows = model.getHeight();
        int cols = model.getWidth();
        boolean[][] visited = new boolean[rows][cols];
        Queue<Location> queue = new LinkedList<>();

        // Add the starting point to the queue but don't add it to the result list
        queue.add(start);
        visited[start.x][start.y] = true;

        int[] directions = { -1, 0, 1, 0, -1 };

        while (!queue.isEmpty() && result.size() < amount) {
            Location current = queue.poll();

            for (int d = 0; d < 4; d++) {
                int newRow = current.x + directions[d];
                int newCol = current.y + directions[d + 1];

                Location newLoc = new Location(newRow, newCol);
                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols
                        && model.isFree(newLoc) && !visited[newRow][newCol]) {
                    queue.add(newLoc);
                    visited[newRow][newCol] = true;
                    result.add(newLoc);
                    if (result.size() == amount) {
                        break;
                    }
                }
            }
        }

        return result;
    }

    public static Location findClosestLocation(List<Location> locations, Location target) {
        if (locations == null || locations.isEmpty() || target == null) {
            throw new IllegalArgumentException("Locations and target must not be null or empty");
        }

        Location closestLocation = null;
        int minDistance = Integer.MAX_VALUE;

        for (Location loc : locations) {
            int distance = loc.distanceChebyshev(target);
            if (distance < minDistance) {
                minDistance = distance;
                closestLocation = loc;
            }
        }

        return closestLocation;
    }
}