package jia;

import jason.asSemantics.*;
import jason.asSyntax.*;
//import java.util.*;
import jason.bb.BeliefBase;

public class get_nearest_sheep_in_range extends DefaultInternalAction {

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        // Get agent's current position and search range
        int agX = (int)((NumberTerm)args[0]).solve();  // Um X und Y-Koordinate des Hundes zu bekommen
        int agY = (int)((NumberTerm)args[1]).solve();
        int range = (int)((NumberTerm)args[4]).solve();  // Suchbereich des Hundes

        // Initialize variables for nearest flock
        int nearestX = -1;  //Initialisierung der Koordinaten des nächstgelegenen Flocks
        int nearestY = -1;
        double nearestDistance = Double.MAX_VALUE;   // Initialisierung der Distanz zum Flock, damit jeder andere Wert der da rein geschrieben wird, auf jeden Fall kleiner ist

        // Get the agent's belief base
        BeliefBase beliefBase = ts.getAg().getBB();  // Holt Beliefbase der Hunde

        // Iterate over all beliefs to find the nearest flock of sheep
        for (Literal belief : beliefBase) { 
            if (belief.getFunctor().equals("sheep_flock") && belief.getArity() == 3) {  //Überprüft ob belief ein flock im gültigen Umreis ist 
                Term flockX = belief.getTerm(0);  //Get X- und Y-Koordinate des flocks
                Term flockY = belief.getTerm(1);
                Term flockSizeTerm = belief.getTerm(2);  //Get Größe des flocks

                if (flockX.isNumeric() && flockY.isNumeric() && flockSizeTerm.isNumeric()) {  //Überprüfung ob Größe des flocks numerisch ist
                    int flockPosX = (int)((NumberTerm)flockX).solve();   //Umwandlung numerisch Wert in X- und Y-Koordinaten
                    int flockPosY = (int)((NumberTerm)flockY).solve();
                    int flockSize = (int)((NumberTerm)flockSizeTerm).solve();   //Umwandlung numerischer Wert in flock Größe

                    double distance = Math.sqrt(Math.pow(agX - flockPosX, 2) + Math.pow(agY - flockPosY, 2));   //Berechnung Entfernung Agent zum Flock

                    if (flockSize > 3 && distance <= range && distance < nearestDistance) {   //Überprüft ob Größe von flock > 3 und innerhalb Suchradius liegt
                        nearestX = flockPosX;  //Setzt die Koordinaten für X und Y
                        nearestY = flockPosY;
                        nearestDistance = distance;   //Updated die Entfernung
                    }
                }
            }
        }

        // Return nearest flock coordinates
        return un.unifies(args[2], new NumberTermImpl(nearestX)) &&   //Gibt Koordinaten des nächstgelegenen SChwarm zurück
               un.unifies(args[3], new NumberTermImpl(nearestY));
    }
}
