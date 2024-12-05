
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

This sadly doesn't work in all cases. It would have been nice...

> Splitting by largest-smallest triangle

This is the approach we use. It is a little bit hard to explain, but basically boils down to calculating
the split position (t-value) that will yield a very small and very large curve triangle and works for all cases.
