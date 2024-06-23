package env.grid.util;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import grid.GridModel;
import grid.util.Pathfinder;
import jason.environment.grid.Location;

public class PathfinderTest {
    @Test
    public void testPathfinderBounds1() {
        GridModel.create("src/test/resources/templates/3_test.txt");
        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.SHEEP);
        Location startPos = new Location(0, 0);
        Location targetPos = new Location(2, 2);
        Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
        assertEquals(nextPos, startPos);
    }

    @Test
    public void testPathfinderBounds2() {
        GridModel.create("src/test/resources/templates/4_test.txt");
        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.SHEEP);
        Location startPos = new Location(4, 4);
        Location targetPos = new Location(2, 2);
        Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
        assertEquals(nextPos, startPos);
    }
}
