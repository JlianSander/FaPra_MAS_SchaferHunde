package service;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;

import grid.GridModel;
import jason.environment.grid.Location;

public class GridBFSTest {
    @Test
    public void TestGridBFS() {
        GridModel.create("src/test/resources/templates/8_test.txt");
        GridModel model = GridModel.getInstance();
        Location wall = new Location(1, 4);
        assertFalse(model.isFree(wall));
        assertTrue(model.getObjectsAt(wall).contains(GridModel.OBSTACLE));

        Location free = new Location(2, 4);
        assertTrue(model.isFree(free));
        assertTrue(model.getObjectsAt(free).contains(GridModel.CLEAN));

        int n = 13;
        List<Location> locations = GridBFS.gatherLocations(free, n);
        assertTrue(locations.size() == n);
        assertTrue(locations.contains(new Location(2, 5)));
        assertTrue(locations.contains(new Location(3, 4)));
        assertTrue(locations.contains(new Location(2, 3)));
        assertTrue(locations.contains(new Location(3, 5)));
        assertTrue(locations.contains(new Location(4, 4)));
        assertTrue(locations.contains(new Location(3, 3)));
        assertTrue(locations.contains(new Location(4, 5)));
        assertTrue(locations.contains(new Location(5, 4)));
        assertTrue(locations.contains(new Location(4, 3)));
        assertTrue(locations.contains(new Location(5, 5)));
        assertTrue(locations.contains(new Location(6, 4)));
        assertTrue(locations.contains(new Location(5, 3)));
        assertTrue(locations.contains(new Location(7, 4)));
    }
}
