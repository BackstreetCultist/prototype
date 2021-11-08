package tech.charliewilkins.prototype.moveMakers;

import org.bytedeco.javacpp.annotation.Platform;

import java.util.Random;

@Platform(library="hyperHeuristic")
public class RandomBitFlip implements MoveMaker{

    @Override
    public String makeMove(String solution) {
        int index = new Random().nextInt(solution.length());
        char[] solutionChars = solution.toCharArray();
        if (solutionChars[index] == '0'){
            solutionChars[index] = '1';
        }
        else {
            solutionChars[index] = '0';
        }
        return String.valueOf(solutionChars);
    }
}
