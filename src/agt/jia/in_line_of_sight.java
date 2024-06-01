package jia;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.Term;

import util.PropertiesLoader;

public class in_line_of_sight extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int AgX = (int) ((NumberTerm) args[0]).solve();
        int AgY = (int) ((NumberTerm) args[1]).solve();
        int TargetX = (int) ((NumberTerm) args[2]).solve();
        int TargetY = (int) ((NumberTerm) args[3]).solve();
        PropertiesLoader loader = PropertiesLoader.getInstance();
        Integer range = loader.getProperty("vision_range", Integer.class);

        // TODO: actual line of sight (respect obstacles)

        if (Math.abs(TargetX - AgX) <= range &&
                Math.abs(TargetY - AgY) <= range) {
            return true;
        } else {
            return false;
        }
    }
}