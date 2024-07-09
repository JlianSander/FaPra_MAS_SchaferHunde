package agt.jia.common;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealVector;
import org.junit.jupiter.api.Test;

import jason.environment.grid.Location;
import jia.util.common.GeometryCalculator;

public class GeometryCalculator_test {

    @Test
    public void calcDirection_inputValid_returnsCorrectVector() throws Exception {
        //testCalcDirection(1, 0);
        // testCalcDirection(0, 1);
        // testCalcDirection(1, 1);
        // testCalcDirection(2, 0);
        // testCalcDirection(2, 1);
        // testCalcDirection(-1, 0);
        // testCalcDirection(0, -1);
        // testCalcDirection(1, -1);
        // testCalcDirection(-1, 1);
        // testCalcDirection(-1, -1);
    }

    private void testCalcDirection(int x, int y) {
        //Arrange
        Location p1 = new Location(0, 0);
        Location p2 = new Location(x, y);
        RealVector expected = MatrixUtils.createRealVector(new double[] { x, y });

        //Act
        RealVector actual = GeometryCalculator.calcDirection(null, p1, p2);

        //Assert
        assertTrue(expected.equals(actual));
    }
}
