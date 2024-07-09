package jia.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Predicate;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.asSyntax.Literal;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.SetTerm;
import jason.asSyntax.SetTermImpl;
import jason.asSyntax.Term;
import jason.environment.grid.Location;

public class QuadrantProcessor {

    public static Location GetCornerOfQ(int q){
        var model = GridModel.getInstance();
        switch(q){
            case 1:
                return new Location(model.getWidth() - 1, 0);
            case 2:
                return new Location(0, 0);
            case 3:
                return new Location(0, model.getHeight() - 1);
            case 4:
                return new Location(model.getWidth() - 1, model.getHeight() - 1);
            default:
                return model.getCenter();
        }
    }

    public static ArrayList<Integer> GetExcludedFromBelief(TransitionSystem ts) {
        var excluded = new ArrayList<Integer>();
        var itLiteral = BeliefBaseManager.getBeliefs(ts, "searchedQuadrants", 1);
        if(itLiteral != null){
            Collection<Term> setBelief = (SetTerm) itLiteral.next().getTerm(0);
            for(var term : setBelief){
                excluded.add((int) ((NumberTermImpl) term).solve());
            }
        }
        return excluded;
    }

    public static Location GetLocToQ(int q){
        var model = GridModel.getInstance();
        int resX = 0, resY = 0;
        switch (q) {
        case 1:
            resX = Math.round(model.getWidth() * (3 / 4f));
            resY = Math.round(model.getHeight() * (1 / 4f));
            break;
        case 2:
            resX = Math.round(model.getWidth() * (1 / 4f));
            resY = Math.round(model.getHeight() * (1 / 4f));
            break;
        case 3:
            resX = Math.round(model.getWidth() * (1 / 4f));
            resY = Math.round(model.getHeight() * (3 / 4f));
            break;
        case 4:
            resX = Math.round(model.getWidth() * (3 / 4f));
            resY = Math.round(model.getHeight() * (3 / 4f));
            break;
        case 5:
            return model.getCenter();
        case 6:
            return new Location(0, 0);
        default:
            throw new RuntimeException();
        }

        return new Location(resX, resY);
    }

    public static int GetQFromLoc(Location loc){
        if(loc.x == 0 && loc.y == 0) return 6;
        if(IsInQ1().test(loc)) return 1;
        if(IsInQ2().test(loc)) return 2;
        if(IsInQ3().test(loc)) return 3;
        if(IsInQ4().test(loc)) return 4;
        return 5;
    }

    public static Predicate<? super Location> IsInQ4() {
        var model = GridModel.getInstance();
        return loc -> loc.x > model.getCenter().x &  loc.y > model.getCenter().y;
    }

    public static Predicate<? super Location> IsInQ3() {
        var model = GridModel.getInstance();
        return loc -> loc.x < model.getCenter().x &  loc.y > model.getCenter().y;
    }

    public static Predicate<? super Location> IsInQ2() {
        var model = GridModel.getInstance();
        return loc -> loc.x < model.getCenter().x &  loc.y < model.getCenter().y;
    }

    public static Predicate<? super Location> IsInQ1() {
        var model = GridModel.getInstance();
        return loc -> loc.x > model.getCenter().x &  loc.y < model.getCenter().y;
    }

    public static void UpdateExcludedInBelief(TransitionSystem ts, ArrayList<Integer> excludedQs) {
        var itLiteral = BeliefBaseManager.getBeliefs(ts, "searchedQuadrants", 1);
        if(itLiteral != null){
            Literal belief = itLiteral.next();
            BeliefBaseManager.removeBelief(ts, belief);
        }

        Collection<Term> setBelief = new SetTermImpl();
        for(var q : excludedQs){
            setBelief.add(new NumberTermImpl(q));
        }

        BeliefBaseManager.addBelief(ts, "searchedQuadrants", null, setBelief);
    }
}
