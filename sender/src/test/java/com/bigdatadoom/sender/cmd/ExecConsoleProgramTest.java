package com.bigdatadoom.sender.cmd;

import org.junit.Test;

/**
 * User: Evgeniy.Chibirev
 */
public class ExecConsoleProgramTest {
    @Test
    public void testExecCommand1() {
        String res = ExecConsoleProgram.exec("ghc -e \"writeFile \\\"D:\\\\Haskell\\\\info\\\\icfpc13\\\\9167u3WZsbuGdyMsABJYXJR3.json\\\" $ json $ orun 5 [\\\"not\\\",\\\"or\\\"]\" run.hs");
        System.out.println("!!!!!!~~~ ~~~~ " + res);
    }
}
