package tech.charliewilkins.prototype.moveAcceptors;

import org.bytedeco.javacpp.annotation.Platform;

import java.util.Random;

@Platform(library="hyperHeuristic")
public class PositiveAcceptor implements MoveAcceptor {

    /**
     * Returns true if the objective value is higher
     * @param baselineObjectiveValue
     * @param newObjectiveValue
     * @return
     */
    @Override
    public boolean acceptMove(int baselineObjectiveValue, int newObjectiveValue) {
        if (newObjectiveValue > baselineObjectiveValue) {
            return true;
        }
        else {
            return false;
        }
    }
}
