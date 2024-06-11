package jia.util;

import grid.GridModel;
import jason.asSemantics.TransitionSystem;
import jason.environment.grid.Location;
import model.AgentInfo;
import service.AgentDB;

public class AgentUtil {
    public static Location getAgentPositionFromTs(TransitionSystem ts) {
        AgentInfo agentInfo = getAgentInfoFromTs(ts);
        return GridModel.getInstance().getAgPos(agentInfo.getCartagoId());
    }

    public static AgentInfo getAgentInfoFromTs(TransitionSystem ts) {
        return AgentDB.getInstance().getAgentByJasonId(ts.getAgArch().getAgName());
    }
}
