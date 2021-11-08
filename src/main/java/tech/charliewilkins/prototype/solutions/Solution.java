package tech.charliewilkins.prototype.solutions;

import org.bytedeco.javacpp.annotation.Platform;

@Platform(library="hyperHeuristic")
public interface Solution {
    public String getSolution();
    public void setSolution(String solution);
    public int getObjectiveValue(String solution);
}
