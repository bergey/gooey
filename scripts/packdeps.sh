for d in TodoMVC bouncing-canvas bouncing-diagrams diagrams-minimal diagrams-reflex-counting diagrams-reflex-follow echo echo-button embed hamlet-static hello timer webgl-01-triangle; do
    packdeps $d/*.cabal;
done
