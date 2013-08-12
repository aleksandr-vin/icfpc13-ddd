package com.bigdatadoom.sender.exception;

/**
 * User: Evgeniy.Chibirev
 */
public class TooManySolutions extends RuntimeException {
    public TooManySolutions(String s) {
        super(s);
    }
    public TooManySolutions() {
        super();
    }
}
