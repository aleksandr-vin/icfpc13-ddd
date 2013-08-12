package com.bigdatadoom.sender.network;

import com.bigdatadoom.sender.dao.ResultsDAO;
import com.bigdatadoom.sender.dao.impl.ResultsDAODumb;
import com.bigdatadoom.sender.exception.StopWithThisProgramException;
import com.bigdatadoom.sender.requests.EvalRequest;
import com.bigdatadoom.sender.requests.GuessRequest;
import com.bigdatadoom.sender.requests.Util;
import org.json.simple.JSONArray;
import org.junit.Ignore;
import org.junit.Test;
/**
 * User: Evgeniy.Chibirev
 */
@Ignore
@SuppressWarnings("unchecked")
public class NetworkTest {
    private String programId = "pOAsuttXKBsOQnvAoB49A17I";

    @Test
    public void testEvalSimple1() {
        JSONArray vectors = Util.generateValuesForEval();


        for (int i=0; i<20; i++) {
            String res = EvalRequest.eval(programId, vectors);
            //assertEquals("ok", res);
            //System.out.println(req.CorrectResult);
        }
    }

    @Ignore
    @Test
    public void testGuessSimple1() {

        ResultsDAO dao = new ResultsDAODumb();


        try {
            for (String code : dao.getProgramsWithTheSameResults(null, "0x0000")) {

                String res = GuessRequest.guess(programId, code);

            //assertEquals("ok", res);
            }
        } catch (StopWithThisProgramException e) {
            System.out.println("We win program or solved in earlier" + programId);
        }
        System.out.println("Ready to go");



    }

}
