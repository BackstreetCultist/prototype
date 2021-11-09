package tech.charliewilkins.prototype;

import org.bytedeco.javacpp.*;
import org.bytedeco.javacpp.annotation.*;
import tech.charliewilkins.prototype.moveAcceptors.*;
import tech.charliewilkins.prototype.moveMakers.*;
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

        public @Name("cppExposedHeuristic") String call(String heuristic, String solutionRepresentation) {
            Solution solution = new MaximiseXSquaredSolution(solutionRepresentation);
            MoveMaker moveMaker = generateMoveMaker(heuristic);
            MoveAcceptor moveAcceptor = generateMoveAcceptor(heuristic);
            Solution newSolution = run(solution, moveMaker, moveAcceptor, 1);
            return (newSolution.getSolution() + newSolution.getObjectiveValue(newSolution.getSolution()));
        }

        private MoveMaker generateMoveMaker(String heuristic) {
            String moveMakerString = heuristic.substring(0, 4);
            int moveMakerCode = Integer.parseInt(moveMakerString, 2);
            switch (moveMakerCode % 4){
                case 0:
                    return new InvertBits();
                case 1:
                    return new RandomBitFlip();
                case 2:
                    return new ReverseSolution();
                default:
                    return new SwapBits();
            }
        }

        private MoveAcceptor generateMoveAcceptor(String heuristic) {
            String moveAcceptorString = heuristic.substring(4, 8);
            int moveAcceptorCode = Integer.parseInt(moveAcceptorString, 2);
            switch (moveAcceptorCode % 4){
                case 0:
                    return new AutomaticAcceptor();
                case 1:
                    return new NaiveAcceptor();
                case 2:
                    return new PositiveAcceptor();
                default:
                    return new PositiveOrEqualAcceptor();
            }
        }

        private Solution run(Solution solution, MoveMaker moveMaker, MoveAcceptor moveAcceptor, int iterations) {
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
            return solution;
        }
    }
}
