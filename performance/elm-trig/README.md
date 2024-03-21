# Angular normalization benchmarks

There are two methods of angular normalization tested here, one that is currently being used in the elm-units library, but has a bug in that it always negates 180 degrees/π radians, and the other that uses (generally expensive) trigonometric functions, but preserves the sign in this edge case.

My runs demonstrated a 5% decrease in efficiency when using the trigonometric functions, which imo is pretty bad. The best option may just be to add a conditional to the normalize function to check for its equivalence, the problem therein is float comparison is always tricky, especially with IEEE 754 standard employed by JavaScript.

