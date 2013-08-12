package com.bigdatadoom.sender.exception;

/**
 * User: Evgeniy.Chibirev
 */
public class StopWithThisProgramException extends  RuntimeException {
    public StopWithThisProgramException(String s) {
        super(s);
    }
    public StopWithThisProgramException() {
        super();
    }
}
