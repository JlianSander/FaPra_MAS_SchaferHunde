package env.grid.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import org.junit.jupiter.api.Test;

import grid.GridModel;
import grid.util.BypassPathfinder;
import grid.util.Pathfinder;
import jason.environment.grid.Location;
import model.AgentInfo;

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

    @Test
    public void testPathfinderBlocked() {
        GridModel model = GridModel.create("src/test/resources/templates/5_test.txt");
        AgentInfo sheep = new AgentInfo(GridModel.SHEEP, 5, "sheep");
        AgentInfo hound = new AgentInfo(GridModel.HOUND, 10, "hound");
        model.initAgent(sheep);
        model.initAgent(hound);
        Location startPos = new Location(0, 0);
        model.setAgPos(sheep, startPos);
        Location targetPos = new Location(4, 0);
        model.setAgPos(hound, targetPos);

        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.SHEEP);
        Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
        assertEquals(startPos, nextPos);
    }

    @Test
    public void testPathfinderNotBlocked() {
        GridModel model = GridModel.create("src/test/resources/templates/5_test.txt");
        AgentInfo sheep = new AgentInfo(GridModel.SHEEP, 5, "sheep");
        AgentInfo hound = new AgentInfo(GridModel.HOUND, 10, "hound");
        model.initAgent(sheep);
        model.initAgent(hound);
        Location startPos = new Location(0, 0);
        model.setAgPos(sheep, startPos);
        Location targetPos = new Location(4, 0);
        model.setAgPos(hound, targetPos);
        targetPos = new Location(4, 1);

        Pathfinder pathfinder = Pathfinder.getInstance(GridModel.SHEEP);
        Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
        assertNotEquals(startPos, nextPos);
    }

    @Test
    public void testPathfinderBypass() {
        GridModel model = GridModel.create("src/test/resources/templates/5_test.txt");
        AgentInfo sheep = new AgentInfo(GridModel.SHEEP, 5, "sheep");
        AgentInfo hound = new AgentInfo(GridModel.HOUND, 10, "hound");
        model.initAgent(sheep);
        model.initAgent(hound);
        Location startPos = new Location(0, 0);
        model.setAgPos(sheep, startPos);
        Location targetPos = new Location(4, 0);
        model.setAgPos(hound, targetPos);

        Pathfinder pathfinder = BypassPathfinder.getInstance();
        Location nextPos = pathfinder.getNextPosition(startPos, targetPos);
        assertNotEquals(startPos, nextPos);
    }
}
