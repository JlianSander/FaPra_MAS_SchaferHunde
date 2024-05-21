package jia.docs;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.StringTermImpl;
import jason.asSyntax.Term;

public class to_be_called extends DefaultInternalAction {
    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        String firstArg = ((StringTermImpl) args[0]).getString();
        ts.getLogger().info("First passed argument: " + firstArg);

        return un.unifies(args[1], new StringTermImpl("First returned arg"))
                && un.unifies(args[2], new NumberTermImpl(42))
                && un.unifies(args[3], new NumberTermImpl((double) 12.34));
    }
}
