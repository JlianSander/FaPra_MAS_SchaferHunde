
package jia.sheep;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import org.javatuples.Pair;

import grid.GridModel;
import grid.util.BypassPathfinder;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.common.in_line_of_sight;
import jia.util.common.AgentUtil;
import service.GridBFS;
import util.PropertiesLoader;

public class flocking_pos extends DefaultInternalAction {
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        // 1. BFS
        // 1a. if BFS == 0 -> stuck
        // 2. Calculate all weights within the vision range
        // 3. Within BFS: Find 'dangerAvoidanceCell' that has the greatest distance to all dangers
        // 4. Within BFS: Find cell with the highest weight
        // 5. Get the pathfinder point between 3 and 4 based on the weights
        // 4a. Within BFS: if sheep are present: Find all neighbors of sheep -> 'preferableCells'
        // 4b. Within BFS: if no sheep are present: add all free cells to 'preferableCells'
        // 5. Within BFS: Go to preferableCell that has largest distance to 'dangerAvoidanceCell'

        GridModel model = GridModel.getInstance();
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);

        //  BFS
        int amount = 10;
        List<Location> reachableLocations = GridBFS.gatherLocations(ownLoc, amount);

        if (reachableLocations.size() == 0) {
            return false;
        }

        // collect all visible cells
        Integer range = PropertiesLoader.getInstance().getProperty("vision_range", Integer.class);
        List<Location> visibleCells = model.getNeighborhood(ownLoc, range, loc -> {
            try {
                Boolean los = (Boolean) new in_line_of_sight().execute(ts, un,
                        new Term[] { new NumberTermImpl(ownLoc.x),
                                new NumberTermImpl(ownLoc.y), new NumberTermImpl(loc.x),
                                new NumberTermImpl(loc.y) });

                return los;
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });

        // calculate weight for each visible cell
        int maxWeight = Integer.MIN_VALUE;
        int minWeight = Integer.MAX_VALUE;
        Map<Location, Integer> cellWeights = new HashMap<>();
        for (Location loc : visibleCells) {
            int weight = getCellWeight(loc);

            if (weight > maxWeight) {
                maxWeight = weight;
            }

            if (weight < minWeight) {
                minWeight = weight;
            }

            cellWeights.put(loc, weight);
        }

        // Filter locations with negative weight
        List<Pair<Integer, Location>> negativeCells = new ArrayList<>();
        for (Map.Entry<Location, Integer> entry : cellWeights.entrySet()) {
            if (entry.getValue() < 0) {
                negativeCells.add(new Pair<>(entry.getValue(), entry.getKey()));
            }
        }
        Location avoidanceCell = getAvoidanceCell(reachableLocations, negativeCells);
        System.out.println("Avoidance cell: " + avoidanceCell);
        if (avoidanceCell.x == -1 && avoidanceCell.y == -1) {
            avoidanceCell = ownLoc;
        }

        // Filter locations with maximum weight
        List<Location> maxCells = new ArrayList<>();
        for (Map.Entry<Location, Integer> entry : cellWeights.entrySet()) {
            if (entry.getValue() == maxWeight) {
                maxCells.add(entry.getKey());
            }
        }
        Location maxCell = getMaxCell(ownLoc, maxCells);

        Location finalCell = getFinalCell(reachableLocations, avoidanceCell, maxCell, minWeight, maxWeight);
        System.out.println("Final cell: " + finalCell);
        return finalCell;
    }

    private int getCellWeight(Location location) {
        int object = GridModel.getInstance().getObjectsAt(location).get(0);
        return getObjectWeight(object);
    }

    private int getObjectWeight(int object) {
        int weight = Integer.MIN_VALUE;
        switch (object) {
            case GridModel.HOUND:
                weight = -5;
                break;
            case GridModel.SHEEP:
                weight = 3;
                break;
            case GridModel.OBSTACLE:
                weight = -1;
                break;
            case GridModel.CLEAN:
                weight = 2;
                break;
            case GridModel.CORRAL:
                weight = 1;
                break;
            default:
                throw new IllegalArgumentException("Invalid object type");
        }
        return weight;
    }

    private Location getAvoidanceCell(List<Location> validCells, List<Pair<Integer, Location>> negativeCells) {
        Location avoidanceCell = new Location(-1, -1);
        double maxDistance = 0;
        for (Location loc : validCells) {
            int distance = 0;
            for (Pair<Integer, Location> pair : negativeCells) {
                int weightedDistance = BypassPathfinder.getInstance().getDistance(loc, pair.getValue1());
                weightedDistance = Math.max(0, Math.min(weightedDistance, Math.abs(pair.getValue0().intValue())));

                distance += weightedDistance;
            }
            if (distance > maxDistance) {
                maxDistance = distance;
                avoidanceCell = loc;
            }
        }
        return avoidanceCell;
    }

    private Location getMaxCell(Location sheepLoc, List<Location> maxCells) {
        // return the cell out of maxCells that has the shortest step distance to the sheep
        Location maxCell = null;
        int minDistance = Integer.MAX_VALUE;
        for (Location loc : maxCells) {
            int distance = BypassPathfinder.getInstance().getDistance(sheepLoc, loc);
            if (distance < minDistance) {
                minDistance = distance;
                maxCell = loc;
            }
        }
        return maxCell;
    }

    private Location getFinalCell(List<Location> validCells, Location avoidanceCell, Location maxCell,
            int avoidanceWeight, int maxWeight) {
        // find the highest possible weight in our class
        int maxPossibleWeight = Integer.MIN_VALUE;
        maxPossibleWeight = Math.max(maxPossibleWeight, Math.abs(getObjectWeight(GridModel.HOUND)));
        maxPossibleWeight = Math.max(maxPossibleWeight, Math.abs(getObjectWeight(GridModel.SHEEP)));
        maxPossibleWeight = Math.max(maxPossibleWeight, Math.abs(getObjectWeight(GridModel.OBSTACLE)));
        maxPossibleWeight = Math.max(maxPossibleWeight, Math.abs(getObjectWeight(GridModel.CLEAN)));
        maxPossibleWeight = Math.max(maxPossibleWeight, Math.abs(getObjectWeight(GridModel.CORRAL)));

        if (avoidanceWeight == Integer.MAX_VALUE) {
            Random rand = new Random();
            avoidanceWeight = (int) (maxPossibleWeight * rand.nextDouble());
        }

        // Normalize the values to a range between 0 and 1 and get avg
        double normalizedNegative = -avoidanceWeight / maxPossibleWeight;
        double normalizedPositive = maxWeight / maxPossibleWeight;
        double avg = (normalizedNegative + normalizedPositive) / 2.0;

        // Get the pathfinder point between the two points based on the weights
        List<Location> path = BypassPathfinder.getInstance().getPath(avoidanceCell, maxCell);
        int index = (int) Math.round(avg * (path.size() - 1));
        for (int i = 1; i < path.size() / 2; i++) {
            if (validCells.contains(path.get(index))) {
                break;
            }

            index = index - i;
            if (validCells.contains(path.get(index))) {
                break;
            }
            index = index + i;
            if (validCells.contains(path.get(index))) {
                break;
            }
        }

        return path.get(index);
    }
}