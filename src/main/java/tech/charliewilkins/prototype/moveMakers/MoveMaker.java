package tech.charliewilkins.prototype.moveMakers;

import org.bytedeco.javacpp.annotation.Platform;

@Platform(library="hyperHeuristic")
public interface MoveMaker {
    public String makeMove(String solution);
}
