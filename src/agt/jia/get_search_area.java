package jia;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.AgentUtil;
import jia.util.BeliefBaseManager;
import jia.util.QuadrantProcessor;

public class get_search_area extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        var posHounds = BeliefBaseManager.getPosHounds(ts);
        var excluded = QuadrantProcessor.GetExcludedFromBelief(ts);
        CheckResetExcluded(ts, excluded);
        int chosenQ = getChosenQ(posHounds, excluded);
        var myLoc = AgentUtil.getAgentPositionFromTs(ts);
        if(chosenQ == QuadrantProcessor.GetQFromLoc(myLoc) & chosenQ != 5 & chosenQ != 6){
            //if already in the quadrant check if another hound is better positioned to search
            if(posHounds.size() > 0){
                var corner = QuadrantProcessor.GetCornerOfQ(chosenQ);
                int distMe = corner.distanceChebyshev(myLoc);
                int distOthersMin = Collections.min(posHounds.stream().map(loc -> corner.distanceChebyshev(loc)).collect(Collectors.toList()));
                if(distOthersMin < distMe){
                    chosenQ = 5;
                }
            }
        }

        var result = QuadrantProcessor.GetLocToQ(chosenQ);
        return un.unifies(args[0], new NumberTermImpl(result.x)) && un.unifies(args[1], new NumberTermImpl(result.y));
    }

    private void CheckResetExcluded(TransitionSystem ts, ArrayList<Integer> excluded) {
        //ts.getLogger().info("get_search_area::CheckResetExcluded --- !!!!!!!!!!!!!!!!!!!!! exculded quadrants: " + excluded.toString());
        if(excluded.contains(1) & excluded.contains(2) & excluded.contains(3) & excluded.contains(4) & excluded.contains(5) & excluded.contains(6)){
            excluded.clear();
            //ts.getLogger().info("get_search_area::CheckResetExcluded --- !!!!!!!!!!!!!!!!!!!!! reseting exculded quadrants with" + excluded.toString());
            QuadrantProcessor.UpdateExcludedInBelief(ts, excluded);
        }
    }

    private int getChosenQ(ArrayList<Location> posHounds, ArrayList<Integer> excludedQs) {
        int numHoundsQ1 = (int) posHounds.stream().filter(QuadrantProcessor.IsInQ1()).count();
        int numHoundsQ2 = (int) posHounds.stream().filter(QuadrantProcessor.IsInQ2()).count();
        int numHoundsQ3 = (int) posHounds.stream().filter(QuadrantProcessor.IsInQ3()).count();
        int numHoundsQ4 = (int) posHounds.stream().filter(QuadrantProcessor.IsInQ4()).count();

        List<Integer> numHounds = Arrays.asList(numHoundsQ1, numHoundsQ2, numHoundsQ3, numHoundsQ4);
        int minNumHounds = Collections.min(numHounds);

        if(minNumHounds > 0 & !excludedQs.contains(5)){
            return 5;
        }else if(numHoundsQ1 == 0 & !excludedQs.contains(1)){
            return 1;
        }else if(numHoundsQ2 == 0 & !excludedQs.contains(2)){
            return 2;
        }else if(numHoundsQ3 == 0 & !excludedQs.contains(3)){
            return 3;
        }else if(numHoundsQ4 == 0 & !excludedQs.contains(4)){
            return 4;
        }else{
            return excludedQs.contains(5) ? 6 : 5;
        }
    }
}
