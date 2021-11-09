package tech.charliewilkins.prototype.moveMakers;

import org.bytedeco.javacpp.annotation.Platform;

import java.util.Random;

@Platform(library="hyperHeuristic")
public class SwapBits implements MoveMaker{

    /**
     * Swap two bits
     * @param solution
     * @return
     */
    @Override
    public String makeMove(String solution) {
        char[] solutionChars = solution.toCharArray();

        return String.valueOf(solutionChars);
    }
}
