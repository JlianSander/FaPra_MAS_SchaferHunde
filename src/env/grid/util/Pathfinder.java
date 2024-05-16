package grid.util;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import jason.environment.grid.Location;
import grid.GridModel;

import org.javatuples.Pair;

import dstarlite.DStarLite;

public class Pathfinder {
    private DStarLite ds;
    private int user;
    private static final List<Pair<Pathfinder, Boolean>> generalInstances = new ArrayList<>();

    private Pathfinder() {
        ds = new DStarLite();
    }

    public synchronized static Pathfinder getInstance(Integer user) {
        Pathfinder pf = null;
        for (Pair<Pathfinder, Boolean> pair : generalInstances) {
            if (!pair.getValue1()) {
                generalInstances.set(generalInstances.indexOf(pair), pair.setAt1(true));
                pf = pair.getValue0();
                break;
            }
        }

        if (pf == null) {
            pf = new Pathfinder();
            generalInstances.add(Pair.with(pf, true));
        }

        pf.user = user;
        return pf;
    }

    public void releaseInstance() {
        for (int i = 0; i < generalInstances.size(); i++) {
            Pair<Pathfinder, Boolean> pair = generalInstances.get(i);
            if (pair.getValue0() == this) {
                generalInstances.set(i, pair.setAt1(false));
                break;
            }
        }
    }

    public Location getNextPosition(Location start, Location target) {
        List<Location> path = getPath(start, target);
        return path.size() > 1 ? path.get(1) : path.get(0);
    }

    private void excludeObstacles() {
        ObstacleMap obstacleMap = GridModel.getInstance().getObstacleMap();
        for (int x = 0; x < GridModel.getInstance().getWidth(); x++) {
            for (int y = 0; y < GridModel.getInstance().getHeight(); y++) {
                ds.updateCell(x, y, obstacleMap.isObstacle(x, y, user) ? -1 : 1);
            }
        }
    }

    public List<Location> getPath(Location start, Location target) {
        ds.init(start.x, start.y, target.x, target.y);
        excludeObstacles();
        ds.replan();
        return ds.getPath().stream().map(s -> new Location(s.x, s.y)).collect(Collectors.toList());
    }
}