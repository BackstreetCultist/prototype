package tech.charliewilkins.prototype.moveMakers;

import org.bytedeco.javacpp.annotation.Platform;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@Platform(library="hyperHeuristic")
public class ReverseSolution implements MoveMaker{

    /**
     * Reverses solution string
     * @param solution
     * @return
     */
    @Override
    public String makeMove(String solution) {
        return String.valueOf(new StringBuilder(solution).reverse().toString());
    }
}
