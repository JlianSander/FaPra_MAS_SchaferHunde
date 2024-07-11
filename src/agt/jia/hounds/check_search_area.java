package jia.hounds;

import java.util.Collections;
import java.util.stream.Collectors;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.common.AgentUtil;
import jia.util.common.BeliefBaseManager;
import jia.util.hounds.QuadrantProcessor;

public class check_search_area extends DefaultInternalAction {
    
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int middleX = (int) ((NumberTerm) args[0]).solve();
            int middleY = (int) ((NumberTerm) args[1]).solve();
            Location middleSearchPattern = new Location(middleX, middleY);
        int chosenQ = QuadrantProcessor.GetQFromLoc(middleSearchPattern);
        if(chosenQ < 5){
            var posHounds = BeliefBaseManager.getPosHounds(ts);
            var myLoc = AgentUtil.getAgentPositionFromTs(ts);
            //ts.getLogger().info("check_search_area --- chosenQ: " + chosenQ);
        
            //if already in the quadrant check if another hound is better positioned to search
            if(posHounds.size() > 0){
                var corner = QuadrantProcessor.GetCornerOfQ(chosenQ);
                //ts.getLogger().info("check_search_area --- corner: " + corner);
                int distMe = corner.distanceChebyshev(myLoc);
                int distOthersMin = Collections.min(posHounds.stream().map(loc -> corner.distanceChebyshev(loc)).collect(Collectors.toList()));
                //ts.getLogger().info("check_search_area --- distOthersMin: " + distOthersMin);
                //ts.getLogger().info("check_search_area --- distMe: " + distMe);
                return distMe <= distOthersMin;
            }
        } 

        return true;
    }
}
