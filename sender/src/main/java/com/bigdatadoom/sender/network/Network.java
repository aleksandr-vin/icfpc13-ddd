package com.bigdatadoom.sender.network;

import com.bigdatadoom.sender.exception.StopWithThisProgramException;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.slf4j.Logger;

import java.io.DataOutputStream;
import java.io.InputStream;
import java.net.*;
import java.util.Scanner;

import static org.slf4j.LoggerFactory.getLogger;

/**
 * User: Evgeniy.Chibirev
 */

@SuppressWarnings("unchecked")
public class Network {
    private static URL myUrl;
    private static HttpURLConnection conn = null;

    private static Logger logger = getLogger(Network.class);


    static String getSecret() {
        return SecretReader.readSecret();
    }

    static String getURL(String x) {
        return "http://icfpc2013.cloudapp.net/" + x + "?auth=" + getSecret();
    }

    public static JSONObject submit(String x, JSONObject request) {
        if (request==null) request = new JSONObject();



        int error = 0;
        JSONObject res=null;

        try {
            myUrl = new URL(getURL(x));

            Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress("proxy.net.billing.ru", 3128));
            Authenticator authenticator = new Authenticator() {

                public PasswordAuthentication getPasswordAuthentication() {
                    return (new PasswordAuthentication("Evgeniy.Chibirev",
                            "wBrogult".toCharArray()));
                }
            };
            Authenticator.setDefault(authenticator);

            conn = (HttpURLConnection) myUrl.openConnection(proxy);


            conn.setRequestMethod("POST");

            conn.setDoOutput(true);
            DataOutputStream wr = new DataOutputStream(conn.getOutputStream());
            wr.writeBytes(request.toString());
            wr.flush();
            wr.close();



            InputStream is = conn.getInputStream();
            Scanner in = new Scanner(is);
            String s = "{}";
            if (in.hasNextLine()) s = in.nextLine();



            JSONParser parser = new JSONParser();

            if (x.equals("myproblems")) {
                JSONArray problemsArray =(JSONArray) parser.parse(s);
                //logger.debug(problemsArray.toString());

                res = new JSONObject();
                res.put("myproblems", problemsArray);

            } else {

                res = (JSONObject) parser.parse(s);
            }

            in.close();

        } catch (Exception e) {
            //e.printStackTrace();
            //logger.debug("Exception in Network");
            logger.debug(" ~~~!! " + e.getMessage());
            error = Integer.parseInt(e.getMessage().substring(36, 39));


        } finally {
            if (conn != null) conn.disconnect();

        }

        //logger.debug(String.valueOf(error));

        switch (error) {
            case 429 : logger.debug(" ~ 429 try again later");
            case 403 : res = submit(x, request);
            break;


            case 410: logger.debug(" ~ 410 problem requested more than 5 minutes ago");;
            case 412: logger.debug(" ~ 412 problem was already solved (by current user)");
                      throw new StopWithThisProgramException(String.valueOf(error));


        }

        return res;
    }



}
