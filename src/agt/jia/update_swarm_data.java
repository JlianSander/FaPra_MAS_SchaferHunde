package jia;

import java.lang.Math;

import jia.util.DrivePositioner;
import jia.util.SwarmManipulator;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class update_swarm_data extends DefaultInternalAction {
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        int centerX = (int) ((NumberTerm) args[0]).solve();
        int centerY = (int) ((NumberTerm) args[1]).solve();
        int size = (int) ((NumberTerm) args[2]).solve();
        int radius = (int) ((NumberTerm) args[3]).solve();
        int newElementX = (int) ((NumberTerm) args[4]).solve();
        int newElementY = (int) ((NumberTerm) args[5]).solve();

        int newCenterX, newCenterY,  newRadius;
        int newSize = size + 1;

        if(size == 0){
            newCenterX = newElementX;
            newCenterY = newElementY;
            newRadius = 0;
        } else{
            newCenterX = Math.round(((centerX * size) + (newElementX)) / (newSize));
            newCenterY = Math.round(((centerY * size) + (newElementY)) / (newSize));

            var center = new Location(centerX, centerY);
            var newElemLoc = new Location(newElementX, newElementY);
            int distance = center.distanceChebyshev(newElemLoc);
            newRadius = distance > radius ? distance : radius;
        }
        
        return un.unifies(args[6], new NumberTermImpl(newCenterX))
                && un.unifies(args[7], new NumberTermImpl(newCenterY))
                && un.unifies(args[8], new NumberTermImpl(newSize))
                && un.unifies(args[9], new NumberTermImpl(newRadius));
    }
}
