
Some notes on the algorithms used to render Beziér curves
=========================================================

Curve-Splitting
---------------

Sometimes curve triangles can be intersected by another point of a shape. In that case the curve triangle has
to be split to avoid artifacts/overdraw. This has proven to be pretty hard, especially when considering the algorithm
should be able to efficiently run in a compute shader. Inside the compute shader, the algorithm has to efficiently
calculate the number of triangles it is gonna be generating before actually traversing the shape, so it can reserve
space accordingly.

> Splitting by closest point to intersecting point

This method is used for most cases, as it yields the best triangles that will be maximally equally sized.

> Splitting by largest-smallest triangle

This is the approach used for more degenerate cases. It is used to calculate a split position (t-value) that works for all cases.
Let's define the Bezier curve using the three points `A, B, C` and call the point inside the curve triangle `P`.
Then we split the curve at the `intersection` of the line `A-P` and the `curve`.
