package jia.hounds;

import java.util.ArrayList;
import java.util.List;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.ObjectTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.common.AgentUtil;
import model.AgentInfo;
import model.State;
import service.AgentDB;
import service.HoundAgent;

public class use_ai extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        GridModel gridModel = GridModel.getInstance();
        HoundAgent houndAgent = HoundAgent.getInstance();
        Location ownLoc = AgentUtil.getAgentPositionFromTs(ts);

        // Get all other agent positions // TODO: limit by los
        List<Location> allSheepPositions = new ArrayList<>();
        List<Location> allHoundPositions = new ArrayList<>();
        for (AgentInfo agInfo : AgentDB.getInstance().getAllAgents()) {
            if (agInfo.getAgentType() == GridModel.SHEEP) {
                allSheepPositions.add(gridModel.getAgPos(agInfo.getCartagoId()));
            } else if (agInfo.getAgentType() == GridModel.HOUND) {
                allHoundPositions.add(gridModel.getAgPos(agInfo.getCartagoId()));
            }
        }

        int sheepAmountLeft = 100; // TODO

        // Compute the current state
        State currentState = houndAgent.computeState(ownLoc, allSheepPositions, allHoundPositions, sheepAmountLeft);
        if (!ownLoc.equals(currentState.getAgentLoc())) {
            System.out.println("PROBLEM!!!!!!!");
        }

        // System.out.println("use AI");
        // System.out.println("ownLoc: " + ownLoc);
        // System.out.println("currentState: " + currentState.getAgentLoc());

        List<Location> availableActions = getAvailableActions(ownLoc);

        // Choose an action
        int action = -1;
        Location newLoc = new Location(-1, -1);
        if (availableActions.size() > 0) {
            newLoc = houndAgent.chooseAction(currentState, availableActions);
            // System.out.println("newLoc: " + newLoc);
            action = 1;
        }

        return un.unifies(args[0], new NumberTermImpl(action))
                && un.unifies(args[1], new ObjectTermImpl(currentState))
                && un.unifies(args[2], new NumberTermImpl(newLoc.x))
                && un.unifies(args[3], new NumberTermImpl(newLoc.y));
    }

    private List<Location> getAvailableActions(Location houndLoc) {
        List<Location> availableActions = new ArrayList<>();
        GridModel gridModel = GridModel.getInstance();

        for (int i = 0; i < 8; i++) {
            Location newLoc = getTargetLocation(houndLoc, i);
            if (gridModel.isFree(newLoc)) {
                availableActions.add(newLoc);
            }
        }
        return availableActions;
    }

    private Location getTargetLocation(Location origin, int direction) {
        int[][] directions = { { -1, 0 }, { -1, 1 }, { 0, 1 }, { 1, 1 }, { 1, 0 }, { 1, -1 }, { 0, -1 }, { -1, -1 } };
        return new Location(origin.x + directions[direction][0], origin.y + directions[direction][1]);
    }
}
