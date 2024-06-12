package jia.util;

import java.lang.Iterable;
import java.util.Iterator;

import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.LiteralImpl;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.StringTermImpl;
import jason.asSyntax.Term;
import jason.asSyntax.PredicateIndicator;
import jason.asSyntax.Literal;
import jason.bb.BeliefBase;

public class BeliefBaseManager {

    public static void addBelief(TransitionSystem ts, String name, Object... terms) {
        try {
            BeliefBase bb = ts.getAg().getBB();
            LiteralImpl literal = new LiteralImpl(name);

            for (Object object : terms) {
                if (object instanceof Integer) {
                    literal.addTerm(new NumberTermImpl((Integer) object));
                } else if (object instanceof Double) {
                    literal.addTerm(new NumberTermImpl((Double) object));
                } else if (object instanceof String){
                    literal.addTerm(new StringTermImpl((String) object));
                }else{
                    throw new Exception("Invalid object type");
                }
            }

            bb.add(literal);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static Iterator<Literal> getBeliefs(TransitionSystem ts, String name, int arity){
        BeliefBase beliefBase = ts.getAg().getBB();
        return beliefBase.getCandidateBeliefs(new PredicateIndicator(name, arity));  
    }

    public static boolean removeBelief(TransitionSystem ts, Literal belief){
        BeliefBase beliefBase = ts.getAg().getBB();
        return beliefBase.remove(belief);
    }
}
