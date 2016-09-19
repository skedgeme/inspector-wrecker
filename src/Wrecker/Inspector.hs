{- | 
  'inspector-wrecker' is a library and executable for creating HTTP benchmarks from
  a HAR file dump from Chrome's Inspector. 
  
  The executable exposes the wrecker options and additionally takes in a path to 
  a HAR file.
  
  The library exposes a single function, 'runHar', which produces a function 
  'wrecker''s library can use for benchmarks.

-}
module Wrecker.Inspector (runHar) where
import Wrecker.Inspector.HAR