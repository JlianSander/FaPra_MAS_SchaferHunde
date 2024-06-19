package env.grid;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import grid.GridModel;

public class GridModelTest {
    @Test
    public void testInLineOfSight() {
        GridModel model = GridModel.create("src/test/resources/templates/1_test.txt");
        assertTrue(model.isFree(1, 1));
        assertTrue(model.isFree(9, 9));
        assertFalse(model.isFree(10, 9));
        assertFalse(model.getObjectsAt(9, 8).contains(GridModel.CORRAL));
        assertTrue(model.getObjectsAt(9, 9).contains(GridModel.CORRAL));
    }
}
