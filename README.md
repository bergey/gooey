GHCJS Examples
==============

[hello](http://bergey.github.io/gooey/)
-----

Hello World example, uses the bare minimum of functions from `ghcjs-dom`
to get text into the browser window.

[embed](http://bergey.github.io/gooey/embed)
-----

Only slightly more elaborate than `hello`, sets a large chunk of static
HTML, and then selects several elements by id. The use of
`documentGetElementById` is important. The use of `embedFile` is not so
important. Both will disappear as I move to better libraries for
constructing HTML.

[hamlet](http://bergey.github.io/gooey/hamlet-static)
------

Use `hamlet` and `blaze-html` to create the inner HTML content.

[echo](http://bergey.github.io/gooey/echo)
----

Use `ghcjs-dom` to read from an input field and echo the value to a
paragraph..

[echo-button](http://bergey.github.io/gooey/echo-button)
-----------

Like **echo**, but register a callback on a button, and only update the
paragraph when the button is pressed. This is the simplest example with
a callback.

timer
-----

A countdown timer using an MVar event queue to tie the knot. Callbacks
on DOM elements add events to the queue. A single Haskell thread pops
events from the queue and updates the (global) state. A `render`
function is called in a loop by `requestAnimationFrame` and updates the
DOM based on the global state. This is the simplest example with an
event queue.

[diagrams-minimal](http://bergey.github.io/gooey/diagrams-minimal/)
----------------

Uses [diagrams-ghcjs](https://github.com/ghcjs/diagrams-ghcjs) and
[ghcjs-canvas](https://github.com/ghcjs/ghcjs-canvas) to render to a
Canvas element.

##  [bouncing-canvas](http://bergey.github.io/gooey/bouncing-canvas/)

Uses [ghcjs-canvas](https://github.com/ghcjs/ghcjs-canvas) to render a
bouncing ball.  Click in the canvas to move the ball to that point.

##  [bouncing-diagrams](http://bergey.github.io/gooey/bouncing-diagrams/)

Uses [diagrams-ghcjs](https://github.com/ghcjs/diagrams-ghcjs) to
render a bouncing ball.  Click in the canvas to move the ball to that
point.
