
Current TODO's and roadmap to keep my sanity
============================================

- curve rendering:
  - add cached instances ✅
  - fix curve splitting
  - fix support for cubic curves
  - write it in a compute shader and benchmark
  - good format for geometry, which supports caching trianglulated data
  - cleanup and good documentation!
  - switch to mathematical coordinate system
  - anti-aliasing ✅
- implement svg support
- fix redrawing (so that it makes sense) ✅
- simple ttf/otf text rendering
- better tests and move full interactive test out of desktop
- touch and touchpad gestures
- reimplement wayland backend with async tasks
- merge egl + gl together and make egl abstract platform details


Testing
=======

Compare my renderer's performance (e.g. on the ghostscript tiger) vs.
1. "femtovg" (Cause I think femtovg looks cool, but I need to find out it has poor performance to justify writing my own).
2. "tiny-skia"
