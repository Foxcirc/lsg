
This crate contains code used for drawing directly to a window provided by the
desktop environment.

The renderers draw filled, curved shapes, if possible on the GPU. Shapes are first
triangulated using ear-clipping and then drawn to the display.

There are different renderers for different platforms.
