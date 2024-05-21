package jia;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class is_in_corral extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        GridModel model = GridModel.getInstance();
        int AgX = (int) ((NumberTerm) args[0]).solve();
        int AgY = (int) ((NumberTerm) args[1]).solve();
        Location agLoc = new Location(AgX, AgY);

        return model.getObjectsAt(agLoc).contains(GridModel.CORRAL);
    }
}
