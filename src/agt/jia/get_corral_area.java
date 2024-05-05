package jia;

import java.util.NoSuchElementException;

import grid.GridModel;
import grid.util.GridProcessor;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTerm;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jason.environment.grid.Area;

public class get_corral_area extends DefaultInternalAction {
    
    private Area corral;

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        try{
           init();
    
            return un.unifies(args[0], new NumberTermImpl((double) this.corral.tl.x))
                        && un.unifies(args[1], new NumberTermImpl((double) this.corral.tl.y))
                        && un.unifies(args[2], new NumberTermImpl((double) this.corral.br.x))
                        && un.unifies(args[3], new NumberTermImpl((double) this.corral.br.y));
        } catch(NoSuchElementException e){
            return false;
        }
    }

    public void init() throws NoSuchElementException{
        GridModel model = GridModel.getInstance();
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        // get random start point of corral
        Location startCorral = gridProcessor.getFirstCell(loc -> model.hasObject(GridModel.CORRAL, loc));
    
        //cover all corral cells with square
        this.corral = new Area(startCorral, startCorral);
        gridProcessor.processEntireGrid(loc -> !this.corral.contains(loc) && model.hasObject(GridModel.CORRAL, loc),
                loc -> this.updateCorral( loc),
                c -> false);
    }

    public Area corral(){
        return this.corral;
    }

    private void updateCorral(Location locToProcess){
        Location topLeft = (this.corral.tl.x > locToProcess.x) || (this.corral.tl.y < locToProcess.y) ? locToProcess : this.corral.tl;
        Location bottomRight = (this.corral.br.x < locToProcess.x) || (this.corral.br.y > locToProcess.y) ? locToProcess : this.corral.br;
        corral = new Area(topLeft, bottomRight);
    }
}
