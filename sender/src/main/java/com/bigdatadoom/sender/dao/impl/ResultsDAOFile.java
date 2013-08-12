package com.bigdatadoom.sender.dao.impl;

import com.bigdatadoom.sender.dao.ResultsDAO;
import com.bigdatadoom.sender.exception.ProgramLostException;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.slf4j.Logger;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

import static org.slf4j.LoggerFactory.getLogger;

/**
 * User: Evgeniy.Chibirev
 */
public class ResultsDAOFile implements ResultsDAO {
    private static Logger logger = getLogger(ResultsDAOFile.class);

    @Override
    public List<String> getProgramsWithTheSameResults(Map<String, List<String>> solutionsWithResults, String correctResults) {
            List<String> filteredSolutions;

            logger.debug(solutionsWithResults.toString());

        if(!solutionsWithResults.containsKey(correctResults))  {
            throw new ProgramLostException(" ~~ No solutions with correct answer after eval request!!!");
        } else {
            filteredSolutions = solutionsWithResults.get(correctResults);
        }

        return filteredSolutions;

    }

    public Map<String, List<String>> getSolutionsWithResults(String filename, int weight) {
        JSONParser parser = new JSONParser();

        try {
            logger.debug(filename);


            HashMap<String, List<String>> solutionsWithResults = new HashMap<String, List<String>>();

            Object obj = parser.parse(new FileReader(String.valueOf(weight) + "\\" + filename));

            JSONArray arr = (JSONArray)obj;

            JSONObject jsonObject = (JSONObject) arr.get(0);

            // loop array
            JSONArray solutions = (JSONArray) jsonObject.get("solutions");
            //logger.debug(" ~~ Number of possible solutions " + solutions.size());

//            Iterator<String> iterator = solutions.iterator();
//            while (iterator.hasNext()) {
//                logger.debug(iterator.next());
//            }

            JSONArray results = (JSONArray) ((JSONObject) arr.get(1)).get("results");
            if (results.size() != solutions.size()) throw new RuntimeException("Bad Data solutions size != results size");

            List<String> value;
            for (int i = 0; i < results.size(); i++) {
                String key = results.get(i).toString();

                if(solutionsWithResults.containsKey(key)) {
                    value = solutionsWithResults.get(key);

                } else {
                    value = new ArrayList<String>();

                }

                value.add(solutions.get(i).toString());
                solutionsWithResults.put(key, value);

            }


            return solutionsWithResults;

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
