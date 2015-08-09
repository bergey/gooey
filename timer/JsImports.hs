{-# LANGUAGE JavaScriptFFI, CPP #-}

-- | Misc FFI imports from JS.

module JsImports where

import GHCJS.Types

#ifdef __GHCJS__
foreign import javascript unsafe "window.setInterval($1, $2);" windowSetInterval :: Double -> JSFun (IO ()) -> IO ()

foreign import javascript unsafe "$r = Date.now();" now :: IO Double

foreign import javascript unsafe
  "var req = window.requestAnimationFrame ||\
             window.mozRequestAnimationFrame ||\
             window.webkitRequestAnimationFrame ||\
             window.msRequestAnimationFrame;\
   var f = function() { $1(); req(f); };\
   req(f);"
  animate :: JSFun (IO ()) -> IO ()
#else
windowSetInterval = error "windowSetInterval: only available from JavaScript"
now = error "now: only available from JavaScript"
animate = error "animate: only available from JavaScript"
#endif
