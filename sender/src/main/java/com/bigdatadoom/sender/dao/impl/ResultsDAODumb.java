package com.bigdatadoom.sender.dao.impl;

import com.bigdatadoom.sender.dao.ResultsDAO;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * User: Evgeniy.Chibirev
 */
public class ResultsDAODumb implements ResultsDAO {

    public List<String> getProgramsWithTheSameResults(String correctResults, String filename, int weight) {
        List<String> results = new ArrayList<String>();
        results.add("(lambda (x) (shr16 (shr16 (shr16 (plus x 1)))))");
        results.add("(lambda (x) (shr16 (shr16 (shr16 (plus x x)))))");
        results.add("(lambda (x) (shr16 (shr16 (shr16 (plus 1 1)))))");
        results.add("(lambda (x) (shr16 (shr16 (shr16 (plus 0 1)))))");

        results.add("(lambda (x) (shr16 (shr16 (plus x (shr16 x)))))");
        results.add("(lambda (x) (shr16 (shr16 (plus 1 (shr16 x)))))");
        results.add("(lambda (x) (shr16 (shr16 (plus 1 (shr16 1)))))");
        results.add("(lambda (x) (shr16 (shr16 (plus x (shr16 1)))))");

        results.add("(lambda (x) (shr16 (plus (shr16 x) (shr16 x))))");
        results.add("(lambda (x) (shr16 (plus (shr16 1) (shr16 x))))");
        results.add("(lambda (x) (shr16 (plus (shr16 x) (shr16 1))))");
        results.add("(lambda (x) (shr16 (plus (shr16 0) (shr16 1))))");
        results.add("(lambda (x) (shr16 (plus (shr16 x) (shr16 0))))");

        results.add("(lambda (x) (shr16 (plus x (plus x x))))");
        results.add("(lambda (x) (shr16 (plus x (plus x 1))))");
        results.add("(lambda (x) (shr16 (plus 1 (plus x 1))))");
        results.add("(lambda (x) (shr16 (plus x (plus 1 1))))");
        results.add("(lambda (x) (shr16 (plus 1 (plus x x))))");

        results.add("(lambda (x) (shr16 (plus x (shr16 (shr16 x)))))");
        results.add("(lambda (x) (shr16 (plus 1 (shr16 (shr16 x)))))");
        results.add("(lambda (x) (shr16 (plus x (shr16 (shr16 0)))))");

        results.add("(lambda (x) (plus (shr16 x) (plus x x)))");
        results.add("(lambda (x) (plus (shr16 x) (plus x 1)))");
        results.add("(lambda (x) (plus (shr16 1) (plus x 1)))");
        results.add("(lambda (x) (plus (shr16 0) (plus x 1)))");

        results.add("(lambda (x) (plus (shr16 x) (shr16 (shr16 x))))");
        results.add("(lambda (x) (plus (shr16 1) (shr16 (shr16 x))))");
        results.add("(lambda (x) (plus (shr16 0) (shr16 (shr16 0))))");

        results.add("(lambda (x) (plus x (plus x (shr16 x))))");
        results.add("(lambda (x) (plus x (plus x (shr16 1))))");
        results.add("(lambda (x) (plus x (plus x (shr16 0))))");
        results.add("(lambda (x) (plus x (plus 1 (shr16 0))))");

        results.add("(lambda (x) (plus x (shr16 (plus x x))))");
        results.add("(lambda (x) (plus x (shr16 (plus x 1))))");
        results.add("(lambda (x) (plus 1 (shr16 (plus x x))))");
        results.add("(lambda (x) (plus 1 (shr16 (plus 1 x))))");

        results.add("(lambda (x) (plus x (shr16 (shr16 (shr16 x)))))");
        results.add("(lambda (x) (plus x (shr16 (shr16 (shr16 1)))))");
        results.add("(lambda (x) (plus x (shr16 (shr16 (shr16 0)))))");




/*        results.add("(lambda (x) (shr1 (not (shr1 x))))");
        results.add("(lambda (x) (shr1 (or x x)))");
        results.add("(lambda (x) (not (or x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");
        results.add("(lambda (x) (shr4 (plus x x)))");

        results.add("(lambda (x) (and (shr4 x) 1))");
        results.add("(lambda (x) (or (shr4 x) 1))");


        results.add("(lambda (x) (shr4 (plus x x)))");*/




        return results;
    }

    @Override
    public Map<String, List<String>> getSolutionsWithResults(String filename, int weight) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public List<String> getProgramsWithTheSameResults(Map<String, List<String>> solutionsWithResults, String correctResults) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
