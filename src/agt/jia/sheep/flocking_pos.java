
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
            Boolean los = (Boolean) new in_line_of_sight().execute(ts, un,
                    new Term[] { new NumberTermImpl(ownLoc.x),
                            new NumberTermImpl(ownLoc.y), new NumberTermImpl(loc.x),
                            new NumberTermImpl(loc.y) });

            return los;
        });

        // calculate weight for each visible cell
        double maxWeight = 0;
        Map<Location, Double> cellWeights = new HashMap<>();
        for (Location loc : visibleCells) {

            int distance = ownLoc.distanceChebyshev(loc);
            double weight = calculateWeight(loc) / distance;

            // Max weight?
            if (weight > maxWeight) {
                maxWeight = weight;
            }

            cellWeights.put(loc, weight);
        }

        // Filter locations with negative weight
        List<Pair<Double, Location>> negativeCells = new ArrayList<>();
        for (Map.Entry<Location, Double> entry : cellWeights.entrySet()) {
            if (entry.getValue() < 0) {
                negativeCells.add(new Pair<>(entry.getValue(), entry.getKey()));
            }
        }
        Location avoidanceCell = getAvoidanceCell(reachableLocations, negativeCells);

        // negativeCells.sort((a, b) -> a.getValue0().compareTo(b.getValue0()));
        // System.out.println("Negative cells: " + negativeCells.size());
        // List<Location> oppositeCells = getOppositeCells(ownLoc, negativeCells);
        // for (Location location : oppositeCells) {
        //     System.out.println("Opposite location: " + location);
        // }

        // Filter locations with maximum weight
        List<Location> maxCells = new ArrayList<>();
        for (Map.Entry<Location, Double> entry : cellWeights.entrySet()) {
            if (entry.getValue() == maxWeight) {
                maxCells.add(entry.getKey());
            }
        }

        // Select a random location from the maxCells
        if (!maxCells.isEmpty()) {
            Location chosenLocation = maxCells.get(new Random().nextInt(maxCells.size()));
            // System.out.println("Chosen positive location: " + chosenLocation);
            oppositeCells.add(chosenLocation);
            Location finalLocation = getAverageLocation(oppositeCells);
            // System.out.println("Final location: " + finalLocation);
            return un.unifies(args[0], new NumberTermImpl(finalLocation.x))
                    && un.unifies(args[1], new NumberTermImpl(finalLocation.y));
        }

        return false;
    }

    private double calculateWeight(Location location) {
        // technically we should check if the list is empty, but I cannot see how that would happen.
        // In any case, an exception would be thrown.
        int object = GridModel.getInstance().getObjectsAt(location).get(0);
        // System.out.println("Testing location: " + location + " with object: " + object);
        double weight = 1.0;
        switch (object) {
            case GridModel.HOUND:
                // System.out.println("!!!!!!!!!!!!!!found a hound at: " + location);
                weight = -5;
                break;
            case GridModel.SHEEP:
                // System.out.println("!!!!!!!!!!!!!!!found a sheep at: " + location);
                weight = 5;
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

    private Location getAvoidanceCell(List<Location> validCells, List<Pair<Double, Location>> negativeCells) {
        Location avoidanceCell = new Location(-1, -1);
        double maxDistance = 0;
        for (Location loc : validCells) {
            int distance = 0;
            for (Pair<Double, Location> pair : negativeCells) {
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

    private List<Location> getOppositeCells(Location agentLocation, List<Pair<Double, Location>> negativeCells) {
        // System.out.println("CALC OPPOSITE. Ag loc: " + agentLocation);
        List<Location> oppositeCells = new ArrayList<>();
        // for every 10 negative value (the first value in the tuple), add the location 1 cell away from the agent
        // i.e. -30 -> add the location 3 cells away from the agent
        // Check if the location is in the grid, if not, reduce the distance by 1 and repeat until the location is the agent's location, then skip
        for (Pair<Double, Location> pair : negativeCells) {
            double weight = pair.getValue0();
            Location loc = pair.getValue1();
            Location oppositeLoc = new Location(-1, -1);
            // System.out.println("Negative location: " + loc + " with weight: " + weight);
            while (weight < 0) {
                oppositeLoc = new Location(agentLocation.x + (loc.x - agentLocation.x) * (int) (weight / 10),
                        agentLocation.y + (loc.y - agentLocation.y) * (int) (weight / 10));

                weight += 10;

                if (GridModel.getInstance().isFree(oppositeLoc)) {
                    // System.out.println("Adding Opposite location: " + oppositeLoc);
                    oppositeCells.add(oppositeLoc);
                    break;
                }
            }
        }

        return oppositeCells;
    }

    private Location getAverageLocation(List<Location> locations) {
        double x = locations.stream().mapToDouble(loc -> loc.x).sum();
        double y = locations.stream().mapToDouble(loc -> loc.y).sum();
        Location averageLocation = new Location((int) Math.round(x / locations.size()),
                (int) Math.round(y / locations.size()));
        return GridModel.getInstance().isFree(averageLocation) ? averageLocation
                : GridModel.getInstance().getFirstFreeNeighbor(averageLocation);
    }
}