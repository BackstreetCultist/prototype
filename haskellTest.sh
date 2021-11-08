cd src/main/java
cp ~/.m2/repository/org/bytedeco/javacpp/1.5.6/javacpp-1.5.6.jar javacpp.jar
javac -cp javacpp.jar tech/charliewilkins/prototype/CppExposedHeuristic.java tech/charliewilkins/prototype/**/*.java
java -jar javacpp.jar tech/charliewilkins/prototype/CppExposedHeuristic -header
cp ../haskell/Main.hs .
ghci -package random Main.hs tech/charliewilkins/prototype/linux-x86_64/libhyperHeuristic.so
