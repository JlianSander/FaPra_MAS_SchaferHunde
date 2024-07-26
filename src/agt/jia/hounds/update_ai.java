package jia.hounds;

import java.util.ArrayList;
import java.util.List;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.ObjectTerm;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.common.in_line_of_sight;
import jia.util.common.AgentUtil;
import model.AgentInfo;
import model.State;
import service.AgentDB;
import service.HoundAgent;

public class update_ai extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        GridModel gridModel = GridModel.getInstance();
        HoundAgent houndAgent = HoundAgent.getInstance();
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);
        State lastState = (State) ((ObjectTerm) args[0]).getObject();

        // Get all other agent positions
        List<Location> allSheepPositions = new ArrayList<>();
        List<Location> allHoundPositions = new ArrayList<>();
        for (AgentInfo agInfo : AgentDB.getInstance().getAllAgents()) {
            if (agInfo.getAgentType() == GridModel.SHEEP) {
                allSheepPositions.add(gridModel.getAgPos(agInfo.getCartagoId()));
            } else if (agInfo.getAgentType() == GridModel.HOUND) {
                allHoundPositions.add(gridModel.getAgPos(agInfo.getCartagoId()));
            }
        }

        List<Location> nearbySheepPositions = new ArrayList<>();
        for (Location sheep : allSheepPositions) {
            if ((Boolean) new in_line_of_sight().execute(ts, un,
                    new Term[] { new NumberTermImpl(ownLoc.x),
                            new NumberTermImpl(ownLoc.y), new NumberTermImpl(sheep.x),
                            new NumberTermImpl(sheep.y) })) {
                nearbySheepPositions.add(sheep);
            }
        }

        int sheepAmountLeft = allSheepPositions.size();
        boolean sheepCaptured = sheepAmountLeft < lastState.getSheepCount();

        // Compute new state
        State newState = houndAgent.computeState(ownLoc, nearbySheepPositions, allHoundPositions,
                sheepAmountLeft);

        // System.out.println("update AI");
        // System.out.println("ownLoc: " + ownLoc);
        // System.out.println("lastState: " + lastState.getAgentLoc());
        // System.out.println("newState: " + newState.getAgentLoc());

        // Update Q-values
        houndAgent.update(lastState, newState, sheepCaptured);
        return true;
    }
}
