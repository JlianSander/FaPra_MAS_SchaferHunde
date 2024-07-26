package service;

import java.io.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import grid.GridModel;
import jason.environment.grid.Location;
import model.State;

public class HoundAgent implements Serializable {
    private static final long serialVersionUID = 1L;

    private String scenario;

    private final ConcurrentHashMap<State, AtomicReference<Map<Location, Double>>> qTables = new ConcurrentHashMap<>();
    private final double learningRate = 0.9;
    private final double discountFactor = 0.9;
    private final double explorationRate = 0.1;
    private final Random random = new Random();

    private final double LIVING_PENALTY = -0.01;

    private static HoundAgent instance;

    private HoundAgent() {
        scenario = "TODO";

        instance = loadFromFile();
        if (instance == null) {
            instance = this;
        }
    }

    public static HoundAgent getInstance() {
        if (instance == null) {
            instance = new HoundAgent();
        }

        return instance;
    }

    public Location chooseAction(State state, List<Location> availableLocations) {
        if (random.nextDouble() < explorationRate) {
            Location randomLocation = availableLocations.get(random.nextInt(availableLocations.size()));
            System.out.println("random");
            return randomLocation;
        } else {
            Location bestLocation = getBestValidLocation(state, availableLocations);
            System.out.println("BEST: " + bestLocation);
            return bestLocation;
        }
    }

    public void update(State oldState, State currentState, boolean sheepCaptured) {
        double reward = sheepCaptured ? 1.0 : LIVING_PENALTY;
        // DEBUG, check if we walked towards the right direction
        // System.out.println(currentState.getAgentLoc() + " vs " + oldState.getAgentLoc());
        // if (currentState.getAgentLoc().x > oldState.getAgentLoc().x
        //         && currentState.getAgentLoc().y == oldState.getAgentLoc().y) {
        if (GridModel.getInstance().getObjectsAt(currentState.getAgentLoc()).contains(GridModel.CORRAL)) {
            System.out.println("Went correct, extra reward");
            reward = 5000.0;
        }
        double oldValue = getQValue(oldState, currentState.getAgentLoc());
        double nextMax = getBestQValue(currentState);
        double newValue = oldValue + learningRate * (reward + discountFactor * nextMax - oldValue);
        setQValue(oldState, currentState.getAgentLoc(), newValue);
    }

    public State computeState(Location houndLoc, List<Location> nearbySheepPositions,
            List<Location> nearbyHoundPositions, int sheepAmountLeft) {
        // Sort entities by distance
        nearbySheepPositions
                .sort(Comparator.comparingInt(a -> Math.abs(a.x - houndLoc.x) + Math.abs(a.y - houndLoc.y)));
        nearbyHoundPositions
                .sort(Comparator.comparingInt(a -> Math.abs(a.x - houndLoc.x) + Math.abs(a.y - houndLoc.y)));

        // Limit the number of entities in the state
        int maxEntitiesInState = 5;
        if (nearbySheepPositions.size() > maxEntitiesInState) {
            nearbySheepPositions = nearbySheepPositions.subList(0, maxEntitiesInState);
        }
        if (nearbyHoundPositions.size() > maxEntitiesInState) {
            nearbyHoundPositions = nearbyHoundPositions.subList(0, maxEntitiesInState);
        }

        return new State(houndLoc, nearbySheepPositions, nearbyHoundPositions, sheepAmountLeft);
    }

    private double getQValue(State state, Location action) {
        return qTables.computeIfAbsent(state, k -> new AtomicReference<>(new HashMap<>()))
                .get().getOrDefault(action, 0.0);
    }

    private void setQValue(State state, Location action, double value) {
        qTables.computeIfAbsent(state, k -> new AtomicReference<>(new HashMap<>()))
                .updateAndGet(qValues -> {
                    qValues.put(action, value);
                    return qValues;
                });
    }

    private Location getBestValidLocation(State state, List<Location> availableActions) {
        Map<Location, Double> qValues = qTables.computeIfAbsent(state, k -> new AtomicReference<>(new HashMap<>()))
                .get();
        Location bestAction = null;
        double bestValue = Double.NEGATIVE_INFINITY;
        Collections.shuffle(availableActions);
        for (Location action : availableActions) {
            double value = qValues.getOrDefault(action, 0.0);
            if (value > bestValue) {
                bestValue = value;
                bestAction = action;
            }
        }
        return bestAction;
    }

    private double getBestQValue(State state) {
        Map<Location, Double> qValues = qTables.computeIfAbsent(state, k -> new AtomicReference<>(new HashMap<>()))
                .get();
        return qValues.values().stream().mapToDouble(Double::doubleValue).max().orElse(0.0);
    }

    public void saveToFile() {
        String filename = "hound+_" + scenario + ".ser";
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(filename))) {
            oos.writeObject(this);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private HoundAgent loadFromFile() {
        String filename = "hound+_" + scenario + ".ser";
        File file = new File(filename);
        if (file.exists()) {
            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(filename))) {
                HoundAgent agent = (HoundAgent) ois.readObject();
                qTables.putAll(agent.qTables);
                return agent;
            } catch (IOException | ClassNotFoundException e) {
                e.printStackTrace();
            }
        }
        return null;
    }
}