package jia;

import jason.NoValueException;
import jason.asSemantics.*;
import jason.asSyntax.*;
import jason.bb.BeliefBase;

public class get_nearest_sheep_in_range_2 extends DefaultInternalAction {

    //*********** 
    //searching for sheeps
    //***********

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        // Get agent's current position and search range
        int agX = (int)((NumberTerm)args[0]).solve();  // Um X- und Y-Koordinate des Hundes zu bekommen
        int agY = (int)((NumberTerm)args[1]).solve();
        int range = (int)((NumberTerm)args[4]).solve();  // Suchbereich des Hundes

        // Initialize variables for nearest flock
        int nearestX = -1;  // Initialisierung der Koordinaten des nächstgelegenen Schafs
        int nearestY = -1;
        double nearestDistance = Double.MAX_VALUE;  // Initialisierung der Distanz zum nächsten Schaf

        // Initialize variable to count the number of sheep in range
        int sheepInRangeCount = 0;

        // Get the agent's belief base
        BeliefBase beliefBase = ts.getAg().getBB();  // Holt Beliefbase der Hunde

        // Debug output: Current beliefs in the agent's belief base
        ts.getAg().getLogger().info("Benni Current beliefs in the agent's belief base:");
        for (Literal belief : beliefBase) {
            ts.getAg().getLogger().info(belief.toString());
        }

        // Iterate over all beliefs to find individual sheep
        for (Literal belief : beliefBase) {
            if (belief.getFunctor().equals("sheep") && belief.getArity() == 1) {  // Überprüft ob belief ein Schaf ist
                String sheepID = belief.getTerm(0).toString();
                int[] sheepPos = getSheepPosition(ts, sheepID); // Holt die Position des Schafes
                int sheepX = sheepPos[0];
                int sheepY = sheepPos[1];

                double distance = Math.sqrt(Math.pow(agX - sheepX, 2) + Math.pow(agY - sheepY, 2));  // Berechnung der Entfernung zum Schaf

                ts.getAg().getLogger().info("Benni Checking sheep at (" + sheepX + ", " + sheepY + ") with distance " + distance);

                if (distance <= range) {  // Überprüft ob Schaf im gültigen Umkreis ist
                    sheepInRangeCount++;  // Zählt die Anzahl der Schafe im Bereich

                    if (distance < nearestDistance) {  // Überprüft ob Schaf das nächste im Bereich ist
                        nearestX = sheepX;  // Setzt die Koordinaten für das nächste Schaf
                        nearestY = sheepY;
                        nearestDistance = distance;  // Updated die Entfernung
                    }
                }
            }
        }

        // Debug output: Number of sheep in range and nearest sheep coordinates
        ts.getAg().getLogger().info("Benni Number of sheep in range: " + sheepInRangeCount);
        ts.getAg().getLogger().info("Benni Nearest sheep coordinates to return: (" + nearestX + ", " + nearestY + ")");

        // If there are at least 3 sheep in range,
        if (sheepInRangeCount >= 3) {
            ts.getAg().getLogger().info("Benni At least 3 sheep found in range. Initiating drive.");
        }

        // Return nearest sheep coordinates
        boolean unifiedX = un.unifies(args[5], new NumberTermImpl(nearestX));
        boolean unifiedY = un.unifies(args[6], new NumberTermImpl(nearestY));

        ts.getAg().getLogger().info("Benni Unification of NearestX: " + unifiedX + ", NearestX value: " + nearestX);
        ts.getAg().getLogger().info("Benni Unification of NearestY: " + unifiedY + ", NearestY value: " + nearestY);

        return unifiedX && unifiedY;
    }

    private int[] getSheepPosition(TransitionSystem ts, String sheepID) {
        //pos_agent(X, Y, sheepID)
        BeliefBase beliefBase = ts.getAg().getBB();
        for (Literal belief : beliefBase) {
            if (belief.getFunctor().equals("pos_agent") && belief.getArity() == 3) {  //Position Schaf
                try {
                    int posX = (int)((NumberTerm)belief.getTerm(0)).solve();
                    int posY = (int)((NumberTerm)belief.getTerm(1)).solve();
                    //String agentID = belief.getTerm(2).toString();
                    String agentID = ((Atom)belief.getTerm(2)).toString();
                    if (agentID.equals(sheepID)) {
                        return new int[]{posX, posY};
                    }
                } catch (NoValueException e) {
                    ts.getAg().getLogger().info("Benni Error retrieving position for sheep " + sheepID + ": " + e.getMessage());  //Abfangbedingung
                }
            }
        }
        return new int[]{0, 0};  // Wenn Schaf nicht gefunden wurden
    }
}
