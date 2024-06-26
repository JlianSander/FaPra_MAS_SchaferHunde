package jia.util;

import java.util.Iterator;

import jason.asSemantics.Agent;
import jason.asSemantics.TransitionSystem;
import jason.asSyntax.Atom;
import jason.asSyntax.LiteralImpl;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.StringTermImpl;
import jason.asSyntax.Term;
import jason.asSyntax.Pred;
import jason.asSyntax.PredicateIndicator;
import jason.asSyntax.Literal;
import jason.bb.BeliefBase;

public class BeliefBaseManager {

    public static void addBelief(TransitionSystem ts, String name, Atom source, Object... terms) {
        try {
            //BeliefBase bb = ts.getAg().getBB();
            Agent agent = ts.getAg();
            LiteralImpl literal = new LiteralImpl(name);

            for (Object object : terms) {
                if (object instanceof Integer) {
                    literal.addTerm(new NumberTermImpl((Integer) object));
                } else if (object instanceof Double) {
                    literal.addTerm(new NumberTermImpl((Double) object));
                } else if (object instanceof String){
                    literal.addTerm(new StringTermImpl((String) object));
                }else if (object instanceof Atom){
                    literal.addTerm((Atom) object);
                }else{
                    throw new Exception("Invalid object type");
                }
            }
            if(source != null){
                Term TSource = Pred.createSource(source);
                literal.addAnnot(TSource);
            }
            agent.addBel(literal);
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
