
package jia.sheep;

import java.util.ArrayList;
import java.util.Collections;
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
    Random rand = new Random();

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        // 1. BFS
        // 1a. if BFS == 0 -> stuck
        // 2. Calculate all weights within the vision range
        // 3. Within BFS: Find 'avoidanceCell' that has the greatest distance to all dangers
        // 4. Within BFS: Find cell with the highest weight (good cell, like another sheep)
        // 5. Get the pathfinder point between 3 and 4 based on the weights

        GridModel model = GridModel.getInstance();
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);
        Integer visionRange = PropertiesLoader.getInstance().getProperty("vision_range", Integer.class);

        //  BFS
        int amount = visionRange * 20;
        List<Location> reachableLocations = GridBFS.gatherLocations(ownLoc, amount);

        if (reachableLocations.size() == 0) {
            return false;
        }

        // collect all visible cells
        List<Location> visibleCells = model.getNeighborhood(ownLoc, visionRange, loc -> {
            try {
                return (Boolean) new in_line_of_sight().execute(ts, un,
                        new Term[] { new NumberTermImpl(ownLoc.x),
                                new NumberTermImpl(ownLoc.y), new NumberTermImpl(loc.x),
                                new NumberTermImpl(loc.y) });
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });

        // calculate weight for each visible cell
        int maxWeight = Integer.MIN_VALUE;
        int avoidanceWeight = Integer.MAX_VALUE;
        Map<Location, Integer> cellWeights = new HashMap<>();
        for (Location loc : visibleCells) {
            int weight = getCellWeight(loc);

            if (weight > maxWeight) {
                maxWeight = weight;
            }

            if (weight < avoidanceWeight && weight < 0) {
                avoidanceWeight = weight;
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

        Location finalCell = getFinalCell(reachableLocations, avoidanceCell, maxCell, avoidanceWeight, maxWeight);
        if (ownLoc.equals(finalCell)) {
            return false;
        }

        return un.unifies(args[0], new NumberTermImpl(finalCell.x))
                && un.unifies(args[1], new NumberTermImpl(finalCell.y));
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
        // scramble the maxCells list
        Collections.shuffle(maxCells);

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
            // avoidanceWeight = -maxPossibleWeight;
            avoidanceWeight = 0;
        }

        // System.out.println("Avoidance cell: " + avoidanceCell);
        // System.out.println("Max cell: " + maxCell);
        // System.out.println("Avoidance weight: " + avoidanceWeight);
        // System.out.println("Max weight: " + maxWeight);

        double avg = scale(avoidanceWeight + maxWeight, -maxPossibleWeight, maxPossibleWeight, 0, 1);

        // Get the pathfinder point between the two points based on the weights
        List<Location> path = BypassPathfinder.getInstance().getPath(avoidanceCell, maxCell);
        int index = (int) Math.round(avg * (path.size() - 1));
        for (int i = 1; i < path.size() / 2; i++) {
            if (validCells.contains(path.get(index))) {
                break;
            }

            index = index - i;
            if (index >= 0 && validCells.contains(path.get(index))) {
                break;
            }
            index = index + i;
            if (index < path.size() && validCells.contains(path.get(index))) {
                break;
            }
        }

        GridModel model = GridModel.getInstance();
        Location finalCell = path.get(index);

        // It's perfectly fine for a sheep to not move, which only happens if the avoidance cell is where the sheep is
        // Majority of the time we want to choose a different cell for variety
        if (!model.isFree(finalCell)) {
            int chance = 70;
            double roll = rand.nextInt(100);
            if (roll <= chance) {
                finalCell = model.getFirstNeighbor(path.get(index), loc -> {
                    return validCells.contains(loc) && model.isFree(loc);
                });
            }
        }

        return finalCell;
    }

    private double scale(double value, double oldMin, double oldMax, double newMin, double newMax) {
        return ((value - oldMin) / (oldMax - oldMin)) * (newMax - newMin) + newMin;
    }
}