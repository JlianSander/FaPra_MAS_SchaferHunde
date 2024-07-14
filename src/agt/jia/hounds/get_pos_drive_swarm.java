package jia.hounds;

import jia.util.common.AgentUtil;
import jia.util.common.GeometryCalculator;
import jia.util.hounds.DriveStrategy1;
import jia.util.hounds.DriveStrategy2;
import jia.util.hounds.DriveStrategy3;
import jia.util.hounds.IDrivePositioner;
import jia.util.hounds.SwarmManipulator;
import jia.util.hounds.ValidatorPos;
import util.PropertiesLoader;

import org.apache.commons.math3.linear.RealVector;
import org.apache.xpath.functions.WrongNumberArgsException;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Area;
import jason.environment.grid.Location;

public class get_pos_drive_swarm extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        //ts.getLogger().info("--------------'get_pos_drive_swarm::execute' ");                                                                             //DEBUG
        int centerX = (int) ((NumberTerm) args[0]).solve();
        int centerY = (int) ((NumberTerm) args[1]).solve();
        int radius = (int) ((NumberTerm) args[2]).solve();
        int positionNumber = (int) ((NumberTerm) args[3]).solve();
        var swarmCenter = new Location(centerX, centerY);
        var swarm = new SwarmManipulator(swarmCenter, radius);

        var get_corral = new get_corral_area();
        get_corral.execute(ts, un, args);
        var corral = get_corral.getCorral();

        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer stratID = loader.getProperty("hound_strategy_drive", Integer.class);
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);

        Location agentLoc;
        if(CheckIfBackToWall(ts, swarm, corral)){
            agentLoc = ValidatorPos.ensurePosValid(ts, ownLoc, swarm.getCenter(), GeometryCalculator.calcDirection(ts, swarm.getCenter(), ownLoc), 0, true);
        }else{
            agentLoc = chooseStrategy(ts, stratID).calculateAgentPosition(ts, ownLoc, swarm, corral,
                positionNumber);
        }

        ts.getLogger().info("--------------'get_pos_drive_swarm::execute' return Location " + agentLoc.toString());                                              //DEBUG
        return un.unifies(args[4], new NumberTermImpl(agentLoc.x))
                && un.unifies(args[5], new NumberTermImpl(agentLoc.y));
    }

    private IDrivePositioner chooseStrategy(TransitionSystem ts, int stratId) throws WrongNumberArgsException {
        switch (stratId) {
            case 1:
                return new DriveStrategy1();
            case 2:
                return new DriveStrategy2();
            case 3:
                return new DriveStrategy3();
            default:
                ts.getAg().getLogger().info("ERROR strategy unknown");
                throw new WrongNumberArgsException("1");
        }
    }

    public static boolean CheckIfBackToWall(TransitionSystem ts, SwarmManipulator swarm, Area corral){
        var model = GridModel.getInstance();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer offsetToSheep = loader.getProperty("hound_keep_distance_to_sheep", Integer.class);
        int distToWall = swarm.getRadius() + offsetToSheep.intValue() + 1;

        RealVector directionSwarm = swarm.getDirectionTo(ts, corral.center());
        //ts.getLogger().info("--------------'DriveStrategyCommon::CheckIfBackToWall' directionSwarm: " + directionSwarm.toString());                                                                  //DEBUG
        RealVector invertedDirection = directionSwarm.mapMultiply(-1.0);
        //ts.getLogger().info("--------------'DriveStrategyCommon::CheckIfBackToWall' invertedDirection: " + invertedDirection.toString());                                                            //DEBUG

        RealVector perpDir = GeometryCalculator.translateAngle(ts, invertedDirection, 90);
        for(int i = 1; i <= distToWall; i++){
            var centerTranslated = GeometryCalculator.translateInDir(ts, swarm.getCenter(), invertedDirection, i);
            for(int j = -swarm.getRadius(); j <= swarm.getRadius(); j++){
                var currentLocInRow = GeometryCalculator.translateInDir(ts, centerTranslated, perpDir, j);
                var objsAtLoc = model.getObjectsAt(currentLocInRow);
                if(objsAtLoc.contains(GridModel.OBSTACLE) || !model.inGrid(currentLocInRow)){
                    //found wall or out of map
                    return true;
                }
            }
        }
        //all cells behind this swarm are potentially accessible
        
        return false;
    }
}
