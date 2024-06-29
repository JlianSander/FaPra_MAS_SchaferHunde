package jia;

import jia.util.DriveStrategy1;
import jia.util.DriveStrategy2;
import jia.util.ExceptionPositioningFailed;
import jia.util.IDrivePositioner;
import jia.util.SwarmManipulator;

import util.PropertiesLoader;

import org.apache.xpath.functions.WrongNumberArgsException;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jason.stdlib.fail;

public class get_pos_drive_swarm extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int centerX = (int) ((NumberTerm) args[0]).solve();
        int centerY = (int) ((NumberTerm) args[1]).solve();
        int radius = (int) ((NumberTerm) args[2]).solve();
        int[] radiusQ = new int[4];
        radiusQ[0] = (int) ((NumberTerm) args[3]).solve();
        radiusQ[1] = (int) ((NumberTerm) args[4]).solve();
        radiusQ[2] = (int) ((NumberTerm) args[5]).solve();
        radiusQ[3] = (int) ((NumberTerm) args[6]).solve();
        int positionNumber = (int) ((NumberTerm) args[7]).solve();
        var swarmCenter = new Location(centerX, centerY);
        var swarm = new SwarmManipulator(swarmCenter, radius, radiusQ);

        var get_corral = new get_corral_area();
        get_corral.execute(ts, un, args);
        var corral = get_corral.getCorral();

        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer stratID = loader.getProperty("hound_strategy_drive", Integer.class);
        try{
            Location agentLoc = chooseStrategy(ts, stratID).calculateAgentPosition(ts, swarm, corral, positionNumber);
            return un.unifies(args[8], new NumberTermImpl(agentLoc.x))
                && un.unifies(args[9], new NumberTermImpl(agentLoc.y));
        }catch(ExceptionPositioningFailed e){
            return fail.create().execute(ts, un, args);
        }
    }

    private IDrivePositioner chooseStrategy(TransitionSystem ts,int stratId) throws WrongNumberArgsException{
        switch(stratId){
            case 1:
                return new DriveStrategy1();
            case 2:
                return new DriveStrategy2();
            default:
            ts.getAg().getLogger().info("ERROR strategy unknown");
            throw new WrongNumberArgsException("1");
        }
    }
}
