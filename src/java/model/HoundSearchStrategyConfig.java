package model;

import util.PropertiesLoader;

public class HoundSearchStrategyConfig {
    private static int strategy = Integer.MIN_VALUE;

    public static int getStrategy() {
        if (strategy == Integer.MIN_VALUE) {
            if (System.getProperty("houndSearchStrategy") != null) {
                strategy = Integer.parseInt(System.getProperty("houndSearchStrategy"));
            } else {
                strategy = PropertiesLoader.getInstance().getProperty("hound_search_strategy",
                        Integer.class);
            }
        }

        return strategy;
    }
}
