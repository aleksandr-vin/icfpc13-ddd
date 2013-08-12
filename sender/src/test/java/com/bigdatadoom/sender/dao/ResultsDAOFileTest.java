package com.bigdatadoom.sender.dao;

import com.bigdatadoom.sender.dao.impl.ResultsDAOFile;
import org.junit.Test;

import java.util.List;
import java.util.Map;

/**
 * User: Evgeniy.Chibirev
 */
public class ResultsDAOFileTest {
    @Test
    public void testFileDAOWithBigFile() {
        long time1 = System.currentTimeMillis();
        ResultsDAO dao = new ResultsDAOFile();

        Map<String,List<String>> solutionsWithResults = dao.getSolutionsWithResults("testSolitionsWithResults.json", 0);
        System.out.println(solutionsWithResults);

        String correctResult =  "[\"0x0000000000000000\",\"0x00000000000000FE\"]";
        List<String> res = dao.getProgramsWithTheSameResults(solutionsWithResults, correctResult);
        System.out.println(res);


        System.out.println("Time spent: " + (System.currentTimeMillis() - time1));






    }

}
