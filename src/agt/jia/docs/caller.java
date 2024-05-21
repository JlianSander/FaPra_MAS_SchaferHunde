package jia.docs;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.StringTermImpl;
import jason.asSyntax.Term;
import jason.asSyntax.VarTerm;

public class caller extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        Unifier u1 = new Unifier();
        StringTermImpl A = new StringTermImpl("Passing an argument here");
        VarTerm B = new VarTerm("B");
        VarTerm C = new VarTerm("C");
        VarTerm D = new VarTerm("D");
        new to_be_called().execute(ts, u1, new Term[] { A, B, C, D });

        // the return values
        ts.getLogger().info("Unifier: " + u1.get("B"));
        ts.getLogger().info("Unifier: " + u1.get("C"));
        ts.getLogger().info("Unifier: " + u1.get("D"));

        return true;
    }
}