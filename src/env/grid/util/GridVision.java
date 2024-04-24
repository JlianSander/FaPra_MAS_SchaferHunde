package grid.util;
import grid.GridModel;
import jason.environment.grid.Location;

/**
 * This class is responsible for calculating the vision of an agent located at a specified point in the grid.
 */
public class GridVision {

    /**
     * This method calculates the agents in the field of view at a specified location, for a given sight.
     * @param loc Location at which the spectator is situated.
     * @param range Range in which the spectator can recognize other agents.
     * @return Bitset containing the idexes of all agents visible from the specified location, within the specified sight.
     */
    public static boolean[] getVisibleAgts(GridModel model, Location loc, int range) {
        boolean[] agtsInVision = new boolean[model.getNbOfAgs()];
        //iterate through square around location
        for(int i = 0; i <= range; i++) {
            for(int j = 0; j <= range; j++) {
                //check for agents in the cells around
                setVisibleAgts(model, agtsInVision, new Location(loc.x - i, loc.y - j));
                setVisibleAgts(model, agtsInVision, new Location(loc.x + i, loc.y - j));
                setVisibleAgts(model, agtsInVision, new Location(loc.x - i, loc.y + j));
                setVisibleAgts(model, agtsInVision, new Location(loc.x + i, loc.y + j));
            }
        }
        
        return agtsInVision;
    }

    //Method sets the agent visible in the specified location in the specified bitset
    private static void setVisibleAgts(GridModel model, boolean[] agtsInVision, Location spectdCell){
        int idxAg = model.getAgAtPos(spectdCell);
        //set bitset if agent is located in the cell
        if(idxAg > -1) {
            agtsInVision[idxAg] = true;
        }
    }
}
