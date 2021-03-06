package tech.charliewilkins.prototype.moveAcceptors;

import org.bytedeco.javacpp.annotation.Platform;

import java.util.Random;


@Platform(library="hyperHeuristic")
public class NaiveAcceptor implements MoveAcceptor {

    /**
     * Returns true if the new objective value is higher, or otherwise with a
     * fifty-fifty chance.
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
            int randomAcceptance = new Random().nextInt(2);
            if (randomAcceptance == 1){
                return true;
            }
            else {
                return false;
            }
        }
    }
}
