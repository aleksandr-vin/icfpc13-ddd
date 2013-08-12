package com.bigdatadoom.sender.requests;

import com.bigdatadoom.sender.network.Network;
import org.json.simple.JSONObject;
import org.slf4j.Logger;

import static org.slf4j.LoggerFactory.getLogger;

/**
 * User: Evgeniy.Chibirev
 */
public class GuessRequest {

    private static Logger logger = getLogger(GuessRequest.class);

    public static String guess(String programId, String program) {
        JSONObject obj = new JSONObject();

        obj.put("id", programId );
        obj.put("program", program);


        JSONObject res = Network.submit("guess", obj);


        String s = "";

        if (res != null) {
            s = res.get("status").toString();
            logger.debug(res.toString());

        }

        if (s.equals("win")) {

            return "ok";
        } else {

            return "-1";
        }
    }

}
