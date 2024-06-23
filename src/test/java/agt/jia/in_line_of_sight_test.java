package agt.jia;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.*;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.in_line_of_sight;
import util.PropertiesLoader;

public class in_line_of_sight_test {
    @Test
    public void testInLineOfSight() throws Exception {
        GridModel model = GridModel.create("src/test/resources/templates/2_test.txt");
        Location start = new Location(8, 3);
        Location obstacle = new Location(start.x + 1, 3);
        assertTrue(model.isFree(start));
        assertTrue(model.getObjectsAt(obstacle).contains(GridModel.OBSTACLE));
        assertFalse(model.isFree(obstacle));

        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer range = loader.getProperty("vision_range", Integer.class);

        for (int i = start.x - range; i <= 3; i++) {
            for (int j = start.y - range; j <= start.y + range; j++) {
                Location inRange = new Location(i, j);
                if (!model.isFree(inRange)) {
                    continue;
                }
                assertTrue(getEvaluation(model, start, inRange));
            }
        }

        // double for loop to check all coords outside the range to be false
        for (int i = start.x - range - 1; i <= start.x + range + 1; i += range * 2) {
            for (int j = start.y - range - 1; j <= start.y + range + 1; j++) {
                Location outOfRange = new Location(i, j);
                if (!model.isFree(outOfRange)) {
                    continue;
                }
                assertFalse(getEvaluation(model, start, outOfRange));
            }
        }
        // Same for loop but for y
        for (int i = start.y - range - 1; i <= start.y + range + 1; i += range * 2) {
            for (int j = start.x - range - 1; j <= start.x + range + 1; j++) {
                Location outOfRange = new Location(i, j);
                if (!model.isFree(outOfRange)) {
                    continue;
                }
                assertFalse(getEvaluation(model, start, outOfRange));
            }
        }

        // Coordinate that is in the range but not in line of sight
        Location toBeExcluded = new Location(start.x + 2, start.y);
        assertFalse(getEvaluation(model, start, toBeExcluded));

        assertTrue(getEvaluation(model, new Location(start.x, 1), new Location(start.x + 1, 0)));
        assertFalse(getEvaluation(model, new Location(start.x, 1), new Location(start.x + 2, 0)));
    }

    private boolean getEvaluation(GridModel model, Location start, Location target) {
        Unifier u1 = new Unifier();
        NumberTermImpl StartX = new NumberTermImpl(start.x);
        NumberTermImpl StartY = new NumberTermImpl(start.y);
        NumberTermImpl TargetX = new NumberTermImpl(target.x);
        NumberTermImpl TargetY = new NumberTermImpl(target.y);

        try {
            Object evaluation = new in_line_of_sight().execute(new TransitionSystem(null, null, null, null), u1,
                    new Term[] { StartX, StartY, TargetX, TargetY });
            return (boolean) evaluation;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
