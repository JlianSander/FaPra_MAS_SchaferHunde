package jia.util;

import java.security.InvalidAlgorithmParameterException;

import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.RealVector;

import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;

public class GeometryCalculator {

    public static RealVector calcDirection(TransitionSystem ts, Location pFrom, Location pTo){
        RealVector direction = MatrixUtils.createRealVector(new double[] {
            pTo.x -  pFrom.x,
            pTo.y -  pFrom.y
        });
        //ts.getLogger().info("--------------'calcDirection' direction not normalized: [" + direction.getEntry(0) + "][" + direction.getEntry(1) + "]");         //DEBUG
        if( direction.getEntry(0) != 0 || direction.getEntry(1) != 0){
            direction = direction.unitVector();
            //ts.getLogger().info("--------------'calcDirection' direction: [" + direction.getEntry(0) + "][" + direction.getEntry(1) + "]");                      //DEBUG
        }

        return direction;
    }

    public static double calcDistanceInDir(Location p1, Location p2, RealVector dir){
        RealVector distance = MatrixUtils.createRealVector( new double[]{
            p2.x - p1.x,
            p2.y - p1.y
        });
        return Math.abs(distance.dotProduct(dir));
    }

    public static double calcStraightSlope(RealVector dir){
        return dir.getEntry(1) / dir.getEntry(0);
    }

    public static double calcStraightIntercept(RealVector dir, Location p){
        // [0,b] = t * dir + p
        // 0 = t * dir.x + p.x
        // t = -p.x / dir.x
        // b = t * dir.y + p.y

        double t = (-1) * p.x / dir.getEntry(0);
        return t * dir.getEntry(1) + p.y;
    }

    public static Location calcStraightIntersection(Location p1, RealVector dir1, Location p2, RealVector dir2) throws InvalidAlgorithmParameterException{
        //check that directions are not parallel
        if(checkParallel(dir1, dir2)){
            throw new InvalidAlgorithmParameterException("The two direction must not be parallel");
        }

        RealVector vertical = MatrixUtils.createRealVector(new double[]{ 0, 1});
        RealVector horizontal = MatrixUtils.createRealVector(new double[]{ 1, 0});
        boolean dir1Parallel = checkParallel(dir1, vertical) || checkParallel(dir1, horizontal);
        boolean dir2Parallel = checkParallel(dir2, vertical) || checkParallel(dir2, horizontal);

        if(dir1Parallel && dir2Parallel){
            return calcStraightIntersectionBothParallel(p1, dir1, p2, dir2);
        }else if(dir1Parallel){
            return calcStraightIntersectionOneParallel(p2, dir2, p1, dir1);
        }else if(dir2Parallel){
            return calcStraightIntersectionOneParallel(p1, dir1, p2, dir2);
        }else{
            return calcStraightIntersectionNormal(p1, dir1, p2, dir2);
        }
    }

    private static Location calcStraightIntersectionBothParallel(Location p1, RealVector dir1, Location p2,
            RealVector dir2) {
        int x = dir1.getEntry(0) == 0 ? p2.x : p1.x;
        int y = dir1.getEntry(1) == 0 ? p2.y : p1.y;
        return new Location(x, y);
    }

    private static Location calcStraightIntersectionOneParallel(Location p1, RealVector straight, Location p2,
            RealVector parallel) {
        double slope = calcStraightSlope(straight);
        double intercept = calcStraightIntercept(straight, p1);
        int x,y;
        if(parallel.getEntry(0) != 0){
            //parallel line is horizontal
            x = p2.x;
            y = Math.round((float)(x * slope + intercept));
        }else{
            //parallel line is vertical
            y = p2.y;
            x = Math.round((float)(y - intercept / slope));
        }

        return new Location(x, y);
    }

    public static boolean checkParallel(RealVector dir1, RealVector dir2) {
        return dir1.getEntry(0)*dir2.getEntry(1) == dir1.getEntry(1)*dir2.getEntry(0);
    }

    private static Location calcStraightIntersectionNormal(Location p1, RealVector dir1, Location p2, RealVector dir2) throws InvalidAlgorithmParameterException{
        double slope1 = calcStraightSlope(dir1);
        double intercept1 = calcStraightIntercept(dir1, p1);
        double slope2 = calcStraightSlope(dir2);
        double intercept2 = calcStraightIntercept(dir2, p2);

        // y = m*x+b
        // m1*x+b1 = m2*x+b2
        // x = (b2-b1)/(m1-m2)
        double x = (intercept2 - intercept1) / (slope1 - slope2);
        double y = x * slope1 + intercept1;

        return new Location(Math.round((float) x), Math.round((float) y));
    }

    public static RealVector translateAngle(TransitionSystem ts, RealVector direction, double angleInDegree){
        double angle = Math.toRadians(angleInDegree);
        //create rotation matrix
        double[][] rotation_entries = {
            { Math.cos(angle), -1 * Math.sin(angle) },
            { Math.sin(angle), Math.cos(angle) }
        };
        // Matrix rotation = new Matrix(rotation_entries);
        RealMatrix rotation = MatrixUtils.createRealMatrix(rotation_entries);
        //ts.getLogger().info("--------------'translateAngle' rotation: [" + rotation_entries[0][0] + " " + rotation_entries[0][1] + "][" + rotation_entries[1][0] + " " + rotation_entries[1][1] + "]");   //DEBUG

        //rotate direction according to angle
        return rotation.operate(direction);
    }

    public static Location translateInDir(TransitionSystem ts, Location p1, RealVector dir, int offset){
        return new Location(p1.x + offset * Math.round((float)dir.getEntry(0)), p1.y + offset * Math.round((float)dir.getEntry(1)));
    }
}
