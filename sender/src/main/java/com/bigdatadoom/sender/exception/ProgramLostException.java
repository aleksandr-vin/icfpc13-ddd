package com.bigdatadoom.sender.exception;

/**
 * User: Evgeniy.Chibirev
 */
public class ProgramLostException extends RuntimeException {
    public ProgramLostException(String s) {
        super(s);
    }
    public ProgramLostException() {
        super();
    }
}
