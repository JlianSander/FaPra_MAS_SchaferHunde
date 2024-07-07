package jia;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import grid.GridModel;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jia.util.BeliefBaseManager;

public class get_search_area extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        var model = GridModel.getInstance();
        var posHounds = BeliefBaseManager.getPosHounds(ts);

        var center = new Location( Math.round(model.getWidth() / 2f), Math.round(model.getHeight() / 2f));
        int numHoundsQ1 = (int) posHounds.stream().filter(loc -> loc.x > center.x &  loc.y < center.y).count();
        int numHoundsQ2 = (int) posHounds.stream().filter(loc -> loc.x < center.x &  loc.y < center.y).count();
        int numHoundsQ3 = (int) posHounds.stream().filter(loc -> loc.x < center.x &  loc.y > center.y).count();
        int numHoundsQ4 = (int) posHounds.stream().filter(loc -> loc.x > center.x &  loc.y > center.y).count();

        List<Integer> numHounds = Arrays.asList(numHoundsQ1, numHoundsQ2, numHoundsQ3, numHoundsQ4);
        int minNumHounds = Collections.min(numHounds);

        int resX = 0, resY = 0;
        if(minNumHounds != 0){
            resX = center.x;
            resY = center.y;
        }else{
            if(numHoundsQ1 == 0 ){
                resX = Math.round(model.getWidth() * (3 / 4f));
                resY = Math.round(model.getHeight() * (1 / 4f));
            }else if(numHoundsQ2 == 0 ){
                resX = Math.round(model.getWidth() * (1 / 4f));
                resY = Math.round(model.getHeight() * (1 / 4f));
            }else if(numHoundsQ3 == 0 ){
                resX = Math.round(model.getWidth() * (1 / 4f));
                resY = Math.round(model.getHeight() * (3 / 4f));
            }else if(numHoundsQ4 == 0 ){
                resX = Math.round(model.getWidth() * (3 / 4f));
                resY = Math.round(model.getHeight() * (3 / 4f));
            }
        }

        return un.unifies(args[0], new NumberTermImpl(resX)) && un.unifies(args[1], new NumberTermImpl(resY));
    }
    
}
