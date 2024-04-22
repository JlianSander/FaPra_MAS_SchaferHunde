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
                drawCorral(g, x, y, Color.blue);
                break;
        }
    }

    public void drawCorral(Graphics g, int x, int y, Color color) {
        g.setColor(color);
        g.fillRect(x * cellSizeW, y * cellSizeH, cellSizeW, cellSizeH);
    }
}