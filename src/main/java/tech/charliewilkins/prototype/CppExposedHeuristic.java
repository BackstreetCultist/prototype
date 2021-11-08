package tech.charliewilkins.prototype;

import org.bytedeco.javacpp.*;
import org.bytedeco.javacpp.annotation.*;
import tech.charliewilkins.prototype.moveAcceptors.MoveAcceptor;
import tech.charliewilkins.prototype.moveAcceptors.NaiveAcceptor;
import tech.charliewilkins.prototype.moveMakers.MoveMaker;
import tech.charliewilkins.prototype.moveMakers.RandomBitFlip;
import tech.charliewilkins.prototype.solutions.MaximiseXSquaredSolution;
import tech.charliewilkins.prototype.solutions.Solution;

import java.util.Random;

@Platform(library="hyperHeuristic")
public class CppExposedHeuristic {
    static { Loader.load(); }

    public static class Callback extends FunctionPointer {
        static { Loader.load(); }
        protected Callback() { allocate(); }
        private native void allocate();

        public @Name("cppExposedHeuristic") int call(String str) {
            return runMaximiseXSquared();
        }

        private int runMaximiseXSquared() {
            String initialSolution = generateInitialSolution();

            MoveAcceptor naiveAcceptor = new NaiveAcceptor();
            MoveMaker randomBitFlip = new RandomBitFlip();
            Solution  maximiseXSquaredSolution = new MaximiseXSquaredSolution(initialSolution);

            return run(maximiseXSquaredSolution, randomBitFlip, naiveAcceptor, 50);
        }

        private int run(Solution solution, MoveMaker moveMaker, MoveAcceptor moveAcceptor, int iterations) {
            int baselineObjectiveValue;
            int newObjectiveValue;
            String baselineSolution;
            String newSolution;
            for (int i = 0; i < iterations; i++){
                System.out.println();
                System.out.println("***RUN " + i + " STARTING***");
                baselineSolution = solution.getSolution();
                baselineObjectiveValue = solution.getObjectiveValue(baselineSolution);
                System.out.println("Initial solution = " + baselineSolution + " with objective value = " + baselineObjectiveValue);
                System.out.println("Making move");
                newSolution = moveMaker.makeMove(solution.getSolution());
                newObjectiveValue = solution.getObjectiveValue(newSolution);
                System.out.println("Proposed solution = " + newSolution + " with objective value = " + newObjectiveValue);
                if (moveAcceptor.acceptMove(baselineObjectiveValue, newObjectiveValue)) {
                    solution.setSolution(newSolution);
                }
            }
            System.out.println();
            System.out.println("***PROGRAM TERMINATED");
            System.out.println("Heuristic solution = " + solution.getSolution() + " with objective value = " + solution.getObjectiveValue(solution.getSolution()));
            return solution.getObjectiveValue(solution.getSolution());
        }

        private String generateInitialSolution() {
            char[] initialSolution = new char[8];
            Random rand = new Random();
            for (int i = 0; i < 8; i++){
                if (rand.nextBoolean() == true){
                    initialSolution[i] = '0';
                }
                else {
                    initialSolution[i] = '1';
                }
            }
            System.out.println("Initial Solution was " + String.valueOf(initialSolution));
            return String.valueOf(initialSolution);
        }
    }
}
