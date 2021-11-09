package tech.charliewilkins.prototype.moveAcceptors;

import org.bytedeco.javacpp.annotation.Platform;

import java.util.Random;

@Platform(library="hyperHeuristic")
public class AutomaticAcceptor implements MoveAcceptor {

    /**
     * Always returns true
     * @param baselineObjectiveValue
     * @param newObjectiveValue
     * @return
     */
    @Override
    public boolean acceptMove(int baselineObjectiveValue, int newObjectiveValue) {
        return true;
    }
}
