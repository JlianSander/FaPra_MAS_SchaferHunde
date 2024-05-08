package jia;

import java.util.NoSuchElementException;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jason.environment.grid.Area;

import jia.get_corral_area;

public class is_in_corral extends DefaultInternalAction {
    
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        try{
            int locX = (int) ((NumberTerm) args[0]).solve();
            int locY = (int) ((NumberTerm) args[1]).solve();
            var loc = new Location(locX, locY);

            var get_corral = new get_corral_area();
            get_corral.init();
            var corral = get_corral.corral();

            System.out.println("Test");

            return corral.contains(loc);
        } catch(NoSuchElementException e){
            return false;
        }
    }
}