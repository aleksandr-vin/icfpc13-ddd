package com.bigdatadoom.sender.requests;

import com.bigdatadoom.sender.network.Network;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.slf4j.Logger;

import java.util.*;

import static org.slf4j.LoggerFactory.getLogger;

/**
 * User: Evgeniy.Chibirev
 */
public class MyProblemsRequest {

    private static Logger logger = getLogger(MyProblemsRequest.class);

    public static Map<String,List<String>> notSolvedproblems(int weight) {
        Map<String, List<String>> suitedProblems = new HashMap<String, List<String>>();

        JSONObject problems = Network.submit("myproblems", null);

        logger.debug(problems.toString());

        if (problems != null) {
            JSONArray problemsArray = (JSONArray) problems.get("myproblems") ;

            Iterator<JSONObject> iterator = problemsArray.iterator();
            while (iterator.hasNext()) {
                JSONObject obj = iterator.next();

                if( (obj.get("solved") == null)
                        && ( Integer.parseInt(obj.get("size").toString()) == weight)
                       // && ( obj.get("operators").toString().indexOf("if0") == -1 )
                       // && ( obj.get("operators").toString().indexOf("\"fold") == -1 )
                  ) {
                    logger.debug("Problems to solve with weight: " + weight + " - " +  obj.toString());
                    suitedProblems.put(obj.get("id").toString(), (List) obj.get("operators"));

                } else {
                    logger.debug(" ~~~~~~~~~~~~~~ !!! " + obj);
                }
            }



            //logger.debug(problems.toString());

        }


        return suitedProblems;
    }

}
