package com.bigdatadoom.sender.dao;

import java.util.List;
import java.util.Map;

/**
 * User: Evgeniy.Chibirev
 */
public interface ResultsDAO {
    public Map<String, List<String>> getSolutionsWithResults(String filename, int weight);
    public List<String> getProgramsWithTheSameResults(Map<String, List<String>> solutionsWithResults, String correctResults);
}
