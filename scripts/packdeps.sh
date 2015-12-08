for pkg in bouncing-canvas bouncing-diagrams diagrams-minimal echo echo-button embed hamlet hello timer TodoMVC; do
    packdeps $pkg/*.cabal
done
