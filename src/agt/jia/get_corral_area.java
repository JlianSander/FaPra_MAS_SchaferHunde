package jia;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;

import grid.GridModel;
import grid.util.GridProcessor;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;
import jason.environment.grid.Area;

public class get_corral_area extends DefaultInternalAction {

    private Area corral;

    public Area getCorral() {
        return corral;
    }

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) {
        List<Location> corralCells = new ArrayList<Location>();
        GridModel model = GridModel.getInstance();
        GridProcessor gridProcessor = new GridProcessor(model.getWidth(), model.getHeight());
        gridProcessor.processEntireGrid(loc -> model.hasObject(GridModel.CORRAL, loc),
                loc -> corralCells.add(loc),
                c -> false);

        corral = new Area(corralCells.get(0), corralCells.get(corralCells.size() - 1));

        return un.unifies(args[0], new NumberTermImpl((double) this.corral.tl.x))
                && un.unifies(args[1], new NumberTermImpl((double) this.corral.tl.y))
                && un.unifies(args[2], new NumberTermImpl((double) this.corral.br.x))
                && un.unifies(args[3], new NumberTermImpl((double) this.corral.br.y));
    }
}
