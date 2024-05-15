package grid.util;

import grid.GridModel;
import grid.GridWorld;
import jason.environment.grid.Location;
import model.AgentInfo;
import org.javatuples.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import dstarlite.DStarLite;

public class Pathfinder implements AgentMoveListener {
    private static GridWorld gridWorld;
    private DStarLite ds;
    private static final Map<Integer, Pathfinder> agentInstances = new HashMap<>();
    private static final List<Pair<Pathfinder, Boolean>> generalInstances = new ArrayList<>();
    private boolean isGeneralInstance = false;

    private Pathfinder() {
        ds = new DStarLite();
        excludeObstacles();
        gridWorld.addAgentMoveListener(this);
    }

    public static void createForAgent(GridWorld gridWorld, AgentInfo agent) {
        if (agentInstances.containsKey(agent.getCartagoId())) {
            throw new IllegalStateException("Pathfinder already exists for agent " + agent.getCartagoId());
        }

        Pathfinder.gridWorld = gridWorld;
        agentInstances.put(agent.getCartagoId(), new Pathfinder());
    }

    public static Pathfinder getInstance(AgentInfo agent) {
        return agentInstances.get(agent.getCartagoId());
    }

    public synchronized static Pathfinder getInstance() {
        for (Pair<Pathfinder, Boolean> pair : generalInstances) {
            if (!pair.getValue1()) {
                generalInstances.set(generalInstances.indexOf(pair), pair.setAt1(true));
                return pair.getValue0();
            }
        }
        Pathfinder pf = new Pathfinder();
        pf.isGeneralInstance = true;
        generalInstances.add(Pair.with(pf, true));
        return pf;
    }

    public synchronized void releaseInstance() {
        for (int i = 0; i < generalInstances.size(); i++) {
            Pair<Pathfinder, Boolean> pair = generalInstances.get(i);
            if (pair.getValue0() == this) {
                generalInstances.set(i, pair.setAt1(false));
                break;
            }
        }
    }

    @Override
    public void onAgentMoved(Location prevLoc, Location newLoc) {
        if (prevLoc != null) {
            ds.updateCell(prevLoc.x, prevLoc.y, 0);
            System.out.println(prevLoc + " is now free");
        }
        ds.updateCell(newLoc.x, newLoc.y, -1);
        System.out.println(newLoc + " is now occupied");
    }

    private void excludeObstacles() {
        GridModel model = GridModel.getInstance();
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        gridProcessor.processEntireGrid(
                loc -> model.hasObject(GridModel.OBSTACLE, loc),
                loc -> ds.updateCell(loc.x, loc.y, -1),
                c -> false);

        excludeOuterBorder();
    }

    // This may cover weird edge cases or may not be necessary at all, who knows
    private void excludeOuterBorder() {
        GridModel model = GridModel.getInstance();
        // Exclude all cells forming a border around the grid, but are not in the grid itself
        for (int i = -1; i <= model.getWidth(); i++) {
            ds.updateCell(i, -1, -1); // Top border
            ds.updateCell(i, model.getHeight(), -1); // Bottom border
        }

        for (int j = -1; j <= model.getHeight(); j++) {
            ds.updateCell(-1, j, -1); // Left border
            ds.updateCell(model.getWidth(), j, -1); // Right border
        }
    }

    public Location getNextPosition(Location start, Location target) {
        List<Location> path = getPath(start, target);
        return path.size() > 1 ? path.get(1) : path.get(0);
    }

    public List<Location> getPath(Location start, Location target) {
        try {
            ds.updateStart(start.x, start.y);
            ds.updateGoal(target.x, target.y);
            System.out.println("PATHFINDER --- Start: " + start + " Goal: " + target);
            ds.replan();
            return ds.getPath().stream().map(s -> new Location(s.x, s.y)).collect(Collectors.toList());
        } finally {
            if (isGeneralInstance) {
                releaseInstance();
            }
        }
    }
}