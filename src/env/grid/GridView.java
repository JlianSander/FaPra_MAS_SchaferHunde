package grid;

import jason.environment.grid.GridWorldView;
import service.AgentDB;
import jason.environment.grid.GridWorldModel;

import java.awt.Graphics;
import java.awt.Color;

public class GridView extends GridWorldView {
    AgentDB agentDB;

    public GridView(GridWorldModel model, AgentDB agentDB) {
        super(model, "Grid World", 600);
        this.agentDB = agentDB;
        setVisible(true);
        repaint();
    }

    @Override
    public void draw(Graphics g, int x, int y, int object) {
        super.draw(g, x, y, object);
        switch (object) {
            case GridModel.OBSTACLE:
                drawObstacle(g, x, y);
                break;
            case GridModel.CORRAL:
                drawFill(g, x, y, Color.GREEN);
                break;
            case GridModel.SHEEP:
                drawAgent(g, x, y, Color.GRAY, object);
                break;
            case GridModel.HOUND:
                drawAgent(g, x, y, Color.RED, object);
                break;
            default:
                throw new IllegalArgumentException("Invalid draw type");
        }
    }

    public void drawFill(Graphics g, int x, int y, Color color) {
        g.setColor(color);
        g.fillRect(x * cellSizeW, y * cellSizeH, cellSizeW, cellSizeH);
    }

    public void drawCircle(Graphics g, int x, int y, Color color) {
        g.setColor(color);
        g.fillOval(x * this.cellSizeW + 2, y * this.cellSizeH + 2, this.cellSizeW - 4, this.cellSizeH - 4);
    }
}