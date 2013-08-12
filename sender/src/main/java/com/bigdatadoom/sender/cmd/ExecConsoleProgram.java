package com.bigdatadoom.sender.cmd;

import org.json.simple.JSONArray;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * User: Evgeniy.Chibirev
 */
public class ExecConsoleProgram {
    public static String exec(String command){

        String[] cmd = {"cmd"};

        Process p = null;
        try {
            p = Runtime.getRuntime().exec(cmd);
        } catch (IOException e) {
            e.printStackTrace();
        }
        new Thread(new SyncPipe(p.getErrorStream(), System.err)).start();
        new Thread(new SyncPipe(p.getInputStream(), System.out)).start();
        PrintWriter stdin = new PrintWriter(p.getOutputStream());
        stdin.println("cd D:\\Haskell\\demo\\icfpc13\\icfpc13-ddd\\");
        stdin.println(command);
        // write any other commands you want here
        stdin.close();
        int returnCode = 0;
        try {
            returnCode = p.waitFor();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return "Return code = " + returnCode;
    }

    public static void createFileForRun(JSONArray data) {
        try {

            FileWriter file = new FileWriter("run.json");
            file.write(data.toJSONString());
            file.flush();
            file.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
