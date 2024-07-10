package jia.common;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import util.PropertiesLoader;

public class in_line_of_sight extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int AgX = (int) ((NumberTerm) args[0]).solve();
        int AgY = (int) ((NumberTerm) args[1]).solve();
        int TargetX = (int) ((NumberTerm) args[2]).solve();
        int TargetY = (int) ((NumberTerm) args[3]).solve();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer range = loader.getProperty("vision_range", Integer.class);

        // Out of range
        if (Math.abs(TargetX - AgX) > range || Math.abs(TargetY - AgY) > range) {
            return false;
        }

        if (!isNotBehindWall(AgX, AgY, TargetX, TargetY)) {
            return false;
        }

        return true;
    }

    // https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
    private boolean isNotBehindWall(int startX, int startY, int targetX, int targetY) {
        int dx = Math.abs(targetX - startX);
        int dy = Math.abs(targetY - startY);
        int sx = startX < targetX ? 1 : -1;
        int sy = startY < targetY ? 1 : -1;
        int err = dx - dy;

        while (true) {
            Location locToCheck = new Location(startX, startY);
            if (GridModel.getInstance().getObjectsAt(locToCheck).contains(GridModel.OBSTACLE)) {
                return false;
            }
            if (startX == targetX && startY == targetY) {
                break;
            }
            int e2 = 2 * err;
            if (e2 > -dy) {
                err -= dy;
                startX += sx;
            }
            if (e2 < dx) {
                err += dx;
                startY += sy;
            }
        }

        return true;
    }
}