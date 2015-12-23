{-# LANGUAGE JavaScriptFFI, CPP #-}

-- | Misc FFI imports from JS.

module JsImports where

import GHCJS.Types
import GHCJS.DOM.Types

#ifdef __GHCJS__
foreign import javascript unsafe "$r = Date.now();" now :: IO Double
foreign import javascript unsafe "window.WindowTimers" windowTimers :: IO WindowTimers
#else
now = error "now: only available from JavaScript"
windowTimers = error "windowTimers: only available from JavaScript"
#endif
