package com.bigdatadoom.sender.requests;

import com.bigdatadoom.sender.network.Network;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

/**
 * User: Evgeniy.Chibirev
 */
public class EvalRequest {

    public static String eval(String programId, JSONArray arguments) {
        JSONObject obj = new JSONObject();

        obj.put("id", programId );
        obj.put("arguments", arguments);


        JSONObject res = Network.submit("eval", obj);
        String s = "";

        if (res != null) {
            s = res.get("status").toString();
        }

        if (s.equals("ok")) {
            return  res.get("outputs").toString();
        } else return "-1";


    }





}
