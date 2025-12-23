
This crate contains a small OpenGL wrapper. The wrapper ensures typesafety and
handles binding automatically. However the functions are usually a 1-to-1 mapping from
the originals functions to the wrapper functions.

The crate is built ontop of the `gl` crate and is meant to replace it in your project.
Right now there are some missing functions but adding new ones is very easy.

See OpenGL documentation at [docs.gl](https://docs.gl).
