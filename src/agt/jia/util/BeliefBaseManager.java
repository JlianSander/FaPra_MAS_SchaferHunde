package jia.util;

import java.util.ArrayList;
import java.util.Iterator;

import grid.GridModel;
import jason.asSemantics.Agent;
import jason.asSemantics.TransitionSystem;
import jason.asSyntax.Atom;
import jason.asSyntax.LiteralImpl;
import jason.asSyntax.NumberTermImpl;
import jason.asSyntax.StringTermImpl;
import jason.asSyntax.Term;
import jason.asSyntax.ListTerm;
import jason.asSyntax.Pred;
import jason.asSyntax.PredicateIndicator;
import jason.asSyntax.SetTerm;
import jason.asSyntax.SetTermImpl;
import jason.asSyntax.Literal;
import jason.bb.BeliefBase;
import jason.environment.grid.Location;
import service.AgentDB;

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
                }else if(object instanceof SetTermImpl){
                    literal.addTerm((SetTermImpl) object);
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

    public static ArrayList<Location> getPosOfSheep(TransitionSystem ts, SwarmManipulator swarm){
        ArrayList<Location> locationsSheep = new ArrayList<Location>();
        Iterator<Literal> itBeliefsSwarms = BeliefBaseManager.getBeliefs(ts, "swarm", 4);
        if(itBeliefsSwarms == null){
            return locationsSheep;
        }

        Iterable<Term> sheepTerms = null;
        while(itBeliefsSwarms.hasNext()){
            Literal literal = itBeliefsSwarms.next();
            Term[] terms = literal.getTermsArray();
            int cX = (int) ((NumberTermImpl) terms[1]).solve();
            int cY = (int) ((NumberTermImpl) terms[2]).solve();
            int r = (int) ((NumberTermImpl) terms[3]).solve();
            if(cX == swarm.getCenter().x && cY == swarm.getCenter().y && r == swarm.getRadius() ){
                if(terms[0] instanceof SetTerm){
                    sheepTerms = (SetTerm) terms[0]; 
                }else if(terms[0] instanceof ListTerm){
                    sheepTerms = (ListTerm) terms[0];
                }else{
                    throw new RuntimeException("Swarm is neither set nor list");
                }
                 
                break;
            }
        }

        for(Term sheepT : sheepTerms){
            Iterator<Literal> itBeliefsPos = BeliefBaseManager.getBeliefs(ts, "pos_agent", 3);
            while(itBeliefsPos.hasNext()){
                Literal literal = itBeliefsPos.next();
                Term[] terms = literal.getTermsArray();
                if(terms[2].equals(sheepT)){
                    int tmpX = (int) ((NumberTermImpl) terms[0]).solve();
                    int tmpY = (int) ((NumberTermImpl) terms[1]).solve();
                    locationsSheep.add(new Location(tmpX, tmpY));
                    break;
                }
            }
        }

        return locationsSheep;
    }

    public static ArrayList<Location> getPosHounds(TransitionSystem ts){
        var locations = new ArrayList<Location>();
        Iterator<Literal> itBeliefsHound = BeliefBaseManager.getBeliefs(ts, "hound", 1);
        if(itBeliefsHound == null){
            return locations;
        }

        while(itBeliefsHound.hasNext()){
            String jasonID = ((Atom) itBeliefsHound.next()).getTerm(0).toString();
            var agentInfo = AgentDB.getInstance().getAgentByJasonId(jasonID);
            locations.add(GridModel.getInstance().getAgPos(agentInfo.getCartagoId()));
        }

        //ts.getLogger().info("BeliefBaseManager::getPosHounds --- locations "  + locations.toString());
        return locations;
    }
}
