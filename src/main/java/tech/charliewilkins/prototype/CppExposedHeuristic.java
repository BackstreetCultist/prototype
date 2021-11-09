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
            System.out.println("\nHeuristic provided was: " + heuristic);
            System.out.println("MoveMaker generated was: " + moveMaker.getClass().getSimpleName());
            System.out.println("MoveAcceptor generated was: " + moveAcceptor.getClass().getSimpleName());
            Solution newSolution = run(solution, moveMaker, moveAcceptor);
            System.out.println("New Solution is: " + newSolution.getSolution());
            System.out.println("New Value is: " + newSolution.getObjectiveValue(newSolution.getSolution()) + "\n");
            return (newSolution.getSolution() + newSolution.getObjectiveValue(newSolution.getSolution()));
        }

        private Solution run(Solution solution, MoveMaker moveMaker, MoveAcceptor moveAcceptor) {
            int baselineObjectiveValue;
            int newObjectiveValue;
            String baselineSolution;
            String newSolution;
            baselineSolution = solution.getSolution();
            System.out.println("Original Solution was: " + baselineSolution);
            baselineObjectiveValue = solution.getObjectiveValue(baselineSolution);
            System.out.println("Original Value was: " + baselineObjectiveValue);
            newSolution = moveMaker.makeMove(solution.getSolution());
            newObjectiveValue = solution.getObjectiveValue(newSolution);
            if (moveAcceptor.acceptMove(baselineObjectiveValue, newObjectiveValue)) {
                solution.setSolution(newSolution);
            }

            return solution;
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
    }
}
