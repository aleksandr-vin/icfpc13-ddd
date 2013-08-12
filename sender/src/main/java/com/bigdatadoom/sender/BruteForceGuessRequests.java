package com.bigdatadoom.sender;

import com.bigdatadoom.sender.dao.ResultsDAO;
import com.bigdatadoom.sender.dao.impl.ResultsDAOFile;
import com.bigdatadoom.sender.exception.ProgramLostException;
import com.bigdatadoom.sender.exception.StopWithThisProgramException;
import com.bigdatadoom.sender.exception.TooManySolutions;
import com.bigdatadoom.sender.requests.EvalRequest;
import com.bigdatadoom.sender.requests.GuessRequest;
import com.bigdatadoom.sender.requests.MyProblemsRequest;
import com.bigdatadoom.sender.requests.Util;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.slf4j.Logger;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import static org.slf4j.LoggerFactory.getLogger;

/**
 * User: Evgeniy.Chibirev
 */
public class BruteForceGuessRequests {
    private static Logger logger = getLogger(BruteForceGuessRequests.class);
    private static ResultsDAO dao;

    public static void main(String[] args) {

        int maxSolutionsLimit = 70;
        int numberOfPossibleSolutions = 0;
        JSONArray vectors = Util.read16BitVectorFromFile();

        dao = new ResultsDAOFile();

        //List of programsId to run
       // Map<String, String> programsToRun = new HashMap<String, String>();
        int weight = 10;
        Map<String, List<String>> programsToRun = MyProblemsRequest.notSolvedproblems(weight);
        //programsToRun.add(args[0]);

        logger.debug(programsToRun.toString());

        String haskellCommand;

        Iterator it = programsToRun.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry pairs = (Map.Entry)it.next();
            JSONObject inputs = new JSONObject();
            inputs.put("inputs", vectors);

            JSONObject size = new JSONObject();
            JSONArray arrSize = new JSONArray();
            arrSize.add(String.valueOf(weight));
            size.put("size", arrSize);

            JSONObject operators = new JSONObject();
            operators.put("operators", pairs.getValue() );


            JSONArray arrayDataForRun = new JSONArray();

            arrayDataForRun.add(inputs);
            arrayDataForRun.add(size);
            arrayDataForRun.add(operators);

            //ExecConsoleProgram.createFileForRun(arrayDataForRun);

            //System.out.println(pairs.getKey() + " = " + pairs.getValue());
            //haskellCommand = "ghc -e \"writeFile \\\"D:\\\\Haskell\\\\info\\\\icfpc13\\\\" + weight + "\\\\" + pairs.getKey() +".json\\\" $ json $ orun " + weight + " " + pairs.getValue().toString().replace("\"","\\\"")  + "\" run.hs";
            haskellCommand = "run D:\\Haskell\\info\\icfpc13\\run.json D:\\Haskell\\info\\icfpc13\\" + weight + "\\" + pairs.getKey() +".json";
            //logger.debug(haskellCommand);

           // ExecConsoleProgram.exec(haskellCommand);


            //it.remove(); // avoids a ConcurrentModificationException
        }

        //ghc -e "writeFile \"solutionsH.json\" $ json $ orun 5 [\"plus\", \"not\"]" run.hs



        String eres = "";
        String gres = "";

        logger.debug(" ~~ Input vectors " + vectors);



/*        for(String programId : new ArrayList<String>()) {*/


        for(String programId : programsToRun.keySet()) {

/*        List<String> testProgram = new ArrayList<String>();
        testProgram.add("FBHABojGQM0g5Q8qXlb1k6Ag");
        for(String programId : testProgram) {*/

           if (programId.equals("zwFWQlq6KthqXfFiupSCBetE"))   continue;
           if (programId.equals("5MIKGkE382p28U23s89fARTi"))   continue;
           if (programId.equals("sXXOpSBftsA0PzeGxVCN8Agl"))   continue;

            logger.debug(" ~~ Solving programId [ " + programId +" ]");
            try {

                Map<String, List<String>> solutionsWithResults = dao.getSolutionsWithResults(programId + ".json", weight);

                //Not used yet!!!
                eres = EvalRequest.eval(programId, vectors);
                logger.debug(" ~~ Correct results " + eres);

                //List<String> results = dao.getProgramsWithTheSameResults(eres, args[1]);
                List<String> results = dao.getProgramsWithTheSameResults(solutionsWithResults, eres);
                //logger.debug(" ~~ our solutions: " + results.toString());

                numberOfPossibleSolutions = results.size();

                //if (numberOfPossibleSolutions > maxSolutionsLimit ) throw new TooManySolutions(numberOfPossibleSolutions + "can't handle so mutch");


                logger.debug(" ~~ Number of possible solutions " + numberOfPossibleSolutions);
                //Collections.rotate(results, results.size());


                logger.debug("========================= Fight!!! ========================");
                for(String result : results) {

                    gres = GuessRequest.guess(programId, result);

                    if(gres == "ok") {

                        logger.debug("\n==================================\n" +
                                "WIN WIN!!!   correct solution for programId: " + programId + "  "
                                + result + "\n==================================\n");
                        throw new StopWithThisProgramException("ok");

                    }

                    numberOfPossibleSolutions--;
                    //if (numberOfPossibleSolutions <= 0) throw new ProgramLostException("mistake in optimization!!");

                    logger.debug("Number of solutions (ALL/DONE): " + results.size() + "/" + numberOfPossibleSolutions);
                }


            } catch (StopWithThisProgramException e) {
                logger.debug("We win program or solved in earlier" + programId);
            } catch (TooManySolutions e){
                logger.debug("Skip program" + programId + ", too many solutions >" + maxSolutionsLimit + " : " + numberOfPossibleSolutions );
            } catch (ProgramLostException e) {
                logger.debug("We lost program" + programId);
                throw new ProgramLostException("Exit!!!! debug Stupid!!");
            }


            logger.debug("Ready to go >>>>>>>>>>>>>>>>>>>");

        }

        logger.debug(" ~~ Finished!");

    }



}
