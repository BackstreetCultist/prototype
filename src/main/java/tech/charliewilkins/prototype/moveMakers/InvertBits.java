package tech.charliewilkins.prototype.moveMakers;

import org.bytedeco.javacpp.annotation.Platform;

import java.util.Random;

@Platform(library="hyperHeuristic")
public class InvertBits implements MoveMaker{

    /**
     * Flips all bits
     * @param solution
     * @return
     */
    @Override
    public String makeMove(String solution) {
        char[] solutionChars = solution.toCharArray();
        for (int i = 0; i < solutionChars.length; i++){
            if (solutionChars[i] == '0'){
                solutionChars[i] = '1';
            }
            else {
                solutionChars[i] = '0';
            }
        }
        return String.valueOf(solutionChars);
    }
}
