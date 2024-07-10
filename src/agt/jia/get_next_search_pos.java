package jia;

import java.util.ArrayList;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

import jia.util.common.AgentUtil;
import jia.util.common.GeometryCalculator;
import jia.util.hounds.ValidatorPos;
import jia.util.hounds.QuadrantProcessor;

public class get_next_search_pos extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int limitIterations = (int) ((NumberTerm) args[0]).solve();
        int middleX = (int) ((NumberTerm) args[1]).solve();
        int middleY = (int) ((NumberTerm) args[2]).solve();
        var middle = new Location(middleX, middleY);
        int offsetBetweenLines = (int) ((NumberTerm) args[3]).solve();
        boolean isInverted = ((int) ((NumberTerm) args[4]).solve()) > 0;

        var model = GridModel.getInstance();
        int posX = middleX;
        int posY = middleY;
        
        if(middle.x == 0 && middle.y == 0){
            //special treatement for Q 6
            posY = limitIterations / (model.getWidth() - 1);
            if(posY % 2 == 0){
                posX = limitIterations % (model.getWidth() - 1);
            }else{
                posX = (model.getWidth() - 1) -  limitIterations % (model.getWidth() - 1);
            }

            if(!model.inGrid(posX, posY)){
                int currentQ = QuadrantProcessor.GetQFromLoc(new Location(middleX, middleY));
                ArrayList<Integer> excludedQs = QuadrantProcessor.GetExcludedFromBelief(ts);
                excludedQs.add(currentQ);
                QuadrantProcessor.UpdateExcludedInBelief(ts, excludedQs);
                return un.unifies(args[5], new NumberTermImpl(-1)) && un.unifies(args[6], new NumberTermImpl(-1)) && un.unifies(args[7], new NumberTermImpl(-1));
            } 
        }
        else{
            int idxPos = 0;
            //calculate till next position in pattern is reached
            while(idxPos < limitIterations + 1){
                int lineLength = offsetBetweenLines + idxPos;
                switch (idxPos % 4) {
                    case 0:
                        posX = isInverted ? posX - lineLength : posX + lineLength;
                        break;
                    case 1:
                        posY = isInverted ? posY - lineLength : posY + lineLength;
                        break;
                    case 2:
                        posX = isInverted ? posX + lineLength : posX - lineLength;
                        break;
                    case 3:
                        posY = isInverted ? posY + lineLength : posY - lineLength;
                        break;            
                    default:
                        break;
                }
                if(!model.inGrid(posX, posY)){
                    int currentQ = QuadrantProcessor.GetQFromLoc(new Location(middleX, middleY));
                    ArrayList<Integer> excludedQs = QuadrantProcessor.GetExcludedFromBelief(ts);
                    excludedQs.add(currentQ);
                    QuadrantProcessor.UpdateExcludedInBelief(ts, excludedQs);
                    return un.unifies(args[5], new NumberTermImpl(-1)) && un.unifies(args[6], new NumberTermImpl(-1)) && un.unifies(args[7], new NumberTermImpl(-1));
                } 

                idxPos++;
            }
        }

        limitIterations++;

        var myLoc = AgentUtil.getAgentPositionFromTs(ts);
        var dirToMiddle = GeometryCalculator.calcDirection(ts, myLoc, middle);
        var dirAwayFromMiddle = dirToMiddle.mapMultiply(-1);
        var result = ValidatorPos.ensurePosValid(ts, myLoc, new Location(posX, posY), dirAwayFromMiddle, 0);

        return un.unifies(args[5], new NumberTermImpl(result.x)) && un.unifies(args[6], new NumberTermImpl(result.y)) && un.unifies(args[7], new NumberTermImpl(limitIterations));
    }
}
