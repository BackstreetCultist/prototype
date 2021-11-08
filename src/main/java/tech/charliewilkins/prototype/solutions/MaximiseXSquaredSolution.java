package tech.charliewilkins.prototype.solutions;

import org.bytedeco.javacpp.annotation.Platform;;

@Platform(library="hyperHeuristic")
public class MaximiseXSquaredSolution implements Solution {
    private String solution;

    public MaximiseXSquaredSolution(String solution) {
        this.solution = solution;
    }

    @Override
    public String getSolution() {
        return solution;
    }

    @Override
    public void setSolution(String solution) {
        this.solution = solution;
    }

    @Override
    public int getObjectiveValue(String solution) {
        int solutionInt = Integer.parseInt(solution, 2);
        return (solutionInt * solutionInt);
    }
}
