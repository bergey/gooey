{-# LANGUAGE JavaScriptFFI, CPP #-}

-- | Misc FFI imports from JS.

module JsImports where

import GHCJS.Types

#ifdef __GHCJS__
foreign import javascript unsafe "$r = Date.now();" now :: IO Double
foreign import javascript unsafe "window.setInterval($1,$2)" js_setInterval :: JSRef a -> Int -> IO Int
#else
now = error "now: only available from JavaScript"
setInterval = error "setInterval: only available from JavaScript"
#endif
