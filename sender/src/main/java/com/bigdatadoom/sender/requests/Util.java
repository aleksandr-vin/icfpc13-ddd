package com.bigdatadoom.sender.requests;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Random;

/**
 * User: Evgeniy.Chibirev
 */
public class Util {


    public static JSONArray generateValuesForEval() {
        JSONArray inputData = new JSONArray();


        for (int i=0; i<256; i++) {
            inputData.add(generateRandom16BitVector());
        }

        return inputData;

    }

    public static String generateRandom16BitVector() {
        char[] chars = "0123456789ABCDEF".toCharArray();
        StringBuilder sb = new StringBuilder();
        Random random = new Random();
        for (int i = 0; i < 16 ; i++) {
            char c = chars[random.nextInt(chars.length)];
            sb.append(c);
        }
        String result = "0x" + sb.toString();
        return result;
    }

    public static JSONArray read16BitVectorFromFile() {
        JSONParser parser = new JSONParser();

        try {

            Object obj = parser.parse(new FileReader("vectors.txt"));

            JSONObject jsonObject = (JSONObject) obj;

            // loop array
            JSONArray vectors = (JSONArray) jsonObject.get("vectors");

            return vectors;

      /*      Iterator<String> iterator = vectors.iterator();
            while (iterator.hasNext()) {
                System.out.println(iterator.next());
            }*/

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (ParseException e) {
            e.printStackTrace();
        }


        return null;
    }



}
