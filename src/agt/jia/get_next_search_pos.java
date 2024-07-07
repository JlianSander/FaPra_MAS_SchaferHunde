package jia;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.AgentUtil;

public class get_next_search_pos extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int limitIterations = (int) ((NumberTerm) args[0]).solve();
        int middleX = (int) ((NumberTerm) args[1]).solve();
        int middleY = (int) ((NumberTerm) args[2]).solve();
        int offsetBetweenLines = (int) ((NumberTerm) args[3]).solve();
        boolean isInverted = ((int) ((NumberTerm) args[4]).solve()) > 0;

        var model = GridModel.getInstance();
        
        int posX = middleX;
        int posY = middleY;
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
            if(!model.inGrid(posX, posY)) break;

            idxPos++;
        }
        limitIterations++;

        var myLoc = AgentUtil.getAgentPositionFromTs(ts);
        var middle = new Location(middleX, middleY);
        var dirToMiddle = GeometryCalculator.calcDirection(ts, myLoc, middle);
        var dirAwayFromMiddle = dirToMiddle.mapMultiply(-1);
        var result = ValidatorPos.ensurePosValid(ts, myLoc, new Location(posX, posY), dirAwayFromMiddle, 0);

        return un.unifies(args[5], new NumberTermImpl(result.x)) && un.unifies(args[6], new NumberTermImpl(result.y)) && un.unifies(args[7], new NumberTermImpl(limitIterations));
    }
}
