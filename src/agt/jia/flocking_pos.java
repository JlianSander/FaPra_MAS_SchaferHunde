package jia;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class flocking_pos extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        GridModel model = GridModel.getInstance();
        int AgX = (int) ((NumberTerm) args[0]).solve();
        int AgY = (int) ((NumberTerm) args[1]).solve();
        Location agLoc = new Location(AgX, AgY);

        // collect all neighboring cells
        int range = 1;
        List<Location> reachableNeighbors = model.getNeighborhood(agLoc, range, loc -> {
            return model.inGrid(loc) && model.isFree(loc);
        });

        // calculate weight for each neighboring cell
        Map<Location, Integer> cellWeights = new HashMap<>();
        for (Location loc : reachableNeighbors) {
            int weight = calculateWeight(loc.x, loc.y);
            cellWeights.put(loc, weight);
        }

        // Find max weight
        int maxWeight = Collections.max(cellWeights.values());

        // Filter locations with maximum weight
        List<Location> maxCells = new ArrayList<>();
        for (Map.Entry<Location, Integer> entry : cellWeights.entrySet()) {
            if (entry.getValue() == maxWeight) {
                maxCells.add(entry.getKey());
            }
        }

        // Select a random location from the maxCells
        if (!maxCells.isEmpty()) {
            Location chosenLocation = maxCells.get(new Random().nextInt(maxCells.size()));
            return un.unifies(args[2], new NumberTermImpl(chosenLocation.x))
                    && un.unifies(args[3], new NumberTermImpl(chosenLocation.y));
        }

        return false;
    }

    private int calculateWeight(int x, int y) {
        // TODO
        return new Random().nextInt(100);
    }
}