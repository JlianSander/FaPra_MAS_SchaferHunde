package jia;

import agt.jia.util.DrivePositioner;
import agt.jia.util.SwarmManipulator;
import grid.GridModel;
import grid.util.Pathfinder;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jason.environment.grid.Area;

import jia.get_corral_area;

public class get_pos_drive_swarm extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int centerX = (int) ((NumberTerm) args[0]).solve();
        int centerY = (int) ((NumberTerm) args[1]).solve();
        int radius = (int) ((NumberTerm) args[2]).solve();
        var swarmCenter = new Location(centerX, centerY);
        var swarm = new SwarmManipulator(swarmCenter, radius);

        var get_corral = new get_corral_area();
        get_corral.init();
        var corral = get_corral.corral();
        
        var agentLoc = DrivePositioner.positionSingleAgent(swarm, corral);

        return un.unifies(args[3], new NumberTermImpl(agentLoc.x))
                    && un.unifies(args[4], new NumberTermImpl(agentLoc.y));
    }
}