package grid;

import jason.environment.grid.GridWorldView;
import jason.environment.grid.GridWorldModel;

import java.awt.Graphics;
import java.awt.Color;

public class GridView extends GridWorldView {
    public GridView(GridWorldModel model) {
        super(model, "Grid World", 600);
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
                drawFill(g, x, y, Color.BLUE);
                break;
            case GridModel.SHEEP:
                drawCircle(g, x, y, Color.GRAY);
                break;
            case GridModel.AGENT:
                drawAgent(g, x, y, Color.BLACK, GridModel.AGENT);
                break;
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