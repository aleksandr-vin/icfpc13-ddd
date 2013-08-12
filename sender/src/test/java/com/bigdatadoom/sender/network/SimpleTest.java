package com.bigdatadoom.sender.network;

import com.bigdatadoom.sender.requests.MyProblemsRequest;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * User: Evgeniy.Chibirev
 */
public class SimpleTest {

    @Ignore
    @Test
    public void test1() {
        List<String> results = new ArrayList<String>();
        results.add("qqq");
        results.add("www");
        results.add("eee");



        for(String res : results) {
            System.out.println(res);
        }

        Collections.rotate(results, 3);

        for(String res : results) {
            System.out.println(res);
        }

    }

    @Ignore
    @Test
    public void testMyProblems() {
        JSONObject problems = Network.submit("myproblems", null);
        JSONArray problemsArray = (JSONArray) problems.get("myproblems") ;

        System.out.println(problems);

    }
    @Test
    public void testMyProblems1() {
        Map<String,List<String>> arr = MyProblemsRequest.notSolvedproblems(5);

        System.out.println(arr);


    }
}
