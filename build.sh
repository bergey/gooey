target=$1
if [ "$target" == "" ]; then
    echo "build.sh needs a build target"
   exit 1
fi
dist=dist/build/$target/$target.jsexe
cabal configure --ghc-options='-DGHCJS_BROWSER'
cabal build $target
# echo "done compiling Haskell; running closure"
# ccjs $dist/all.js --compilation_level=ADVANCED_OPTIMIZATIONS  > $dist/all.min.js
cp $target/static/* $dist
warp -d $dist -p 8000
