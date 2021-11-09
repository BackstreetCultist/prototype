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
        int index1 = new Random().nextInt(solution.length());
        int index2 = new Random().nextInt(solution.length());
        char[] solutionChars = solution.toCharArray();
        char buffer = solutionChars[index1];
        solutionChars[index1] = solutionChars[index2];
        solutionChars[index2] = buffer;
        return String.valueOf(solutionChars);
    }
}
