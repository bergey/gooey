GHCJS Examples
==============

hello
-----

Hello World example, uses the bare minimum of functions from `ghcjs-dom`
to get text into the browser window.

embed
-----

Only slightly more elaborate than `hello`, sets a large chunk of static
HTML, and then selects several elements by id. The use of
`documentGetElementById` is important. The use of `embedFile` is not so
important. Both will disappear as I move to better libraries for
constructing HTML.

hamlet
------

Use `hamlet` and `blaze-html` to create the inner HTML content.

echo
----

Use `ghcjs-dom` to read from an input field and echo the value to a
paragraph..

echo-button
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

diagrams-minimal
----------------

Uses [diagrams-ghcjs](https://github.com/ghcjs/diagrams-ghcjs) and
[ghcjs-canvas](https://github.com/ghcjs/ghcjs-canvas) to render to a
Canvas element. Also uses
[ghcjs-jquery](https://github.com/ghcjs/ghcjs-jquery) to setup the
initial DOM. There's probably a better way without jquery, however.
