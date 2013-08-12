package com.bigdatadoom.sender.network;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * User: Evgeniy.Chibirev
 */
public class SecretReader {
    static final String DEFAULT_FILE = "secret.txt";

    public static String readSecret() {
        return readSecret(DEFAULT_FILE);
    }

    public static String readSecret(String fileName) {
        try {
            return new BufferedReader(new FileReader(fileName)).readLine().trim();
        } catch (IOException e) {
            return "";
        }
    }
}
