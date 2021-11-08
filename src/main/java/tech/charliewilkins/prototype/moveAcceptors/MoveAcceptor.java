package tech.charliewilkins.prototype.moveAcceptors;

import org.bytedeco.javacpp.annotation.Platform;

@Platform(library="hyperHeuristic")
public interface MoveAcceptor {
    public boolean acceptMove(int baselineObjectiveValue, int newObjectiveValue);
}
