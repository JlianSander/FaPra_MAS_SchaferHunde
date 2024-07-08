package simulations;

import jacamo.infra.JaCaMoLauncher;
import jason.JasonException;

public class SimStarter {
    public static void main(String[] args) throws JasonException {
        System.out.println("hello");
        for (String string : args) {
            System.out.println(string);
        }

        JaCaMoLauncher.main(args);
    }
}
