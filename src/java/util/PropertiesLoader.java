package util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;

public class PropertiesLoader {
    public class PropertyNotFoundException extends RuntimeException {
        public PropertyNotFoundException(String message) {
            super(message);
        }
    }

    public class UnsupportedPropertyTypeException extends RuntimeException {
        public UnsupportedPropertyTypeException(String message) {
            super(message);
        }
    }

    public class InvalidTypeCastException extends RuntimeException {
        public InvalidTypeCastException(String message) {
            super(message);
        }
    }

    private Properties prop;

    private static PropertiesLoader loader = new PropertiesLoader();

    public static PropertiesLoader getInstance() {
        return loader;
    }

    private PropertiesLoader() {
        String path = "config.properties";
        prop = new Properties();
        try {
            prop.load(new FileInputStream(new File(path)));
        } catch (FileNotFoundException e) {
            throw new IllegalArgumentException("Properties file not found: " + path, e);
        } catch (IOException e) {
            throw new IllegalArgumentException("Error reading properties file: " + path, e);
        }
    }

    public synchronized <T> T getProperty(String key, Class<T> type) {
        String property = prop.getProperty(key);

        if (property == null) {
            throw new PropertyNotFoundException("Property " + key + " not found.");
        }

        if (type == String.class) {
            return type.cast(property);
        } else if (type == Integer.class) {
            return type.cast(parseInteger(property));
        } else if (type == Double.class) {
            return type.cast(parseDouble(property));
        } else if (type == Boolean.class) {
            return type.cast(parseBoolean(property));
        } else {
            throw new UnsupportedPropertyTypeException("Unsupported property type: " + type);
        }
    }

    private Integer parseInteger(String property) {
        if ("Integer.MAX_VALUE".equals(property)) {
            return Integer.MAX_VALUE;
        } else {
            try {
                return Integer.parseInt(property);
            } catch (NumberFormatException e) {
                throw new InvalidTypeCastException("Invalid integer value: " + property);
            }
        }
    }

    private Double parseDouble(String property) {
        if ("Double.MAX_VALUE".equals(property)) {
            return Double.MAX_VALUE;
        } else {
            try {
                return Double.parseDouble(property);
            } catch (NumberFormatException e) {
                throw new InvalidTypeCastException("Invalid double value: " + property);
            }
        }
    }

    private Boolean parseBoolean(String property) {
        if ("true".equals(property.toLowerCase()) || "false".equals(property.toLowerCase())) {
            return Boolean.parseBoolean(property);
        } else {
            throw new InvalidTypeCastException("Invalid boolean value: " + property);
        }
    }
}