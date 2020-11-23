module Web.BingLens where

import Control.Lens 


_resourceSet :: Lens' BingResourceSet [BingLocationResource]
_resourceSet = lens resourceSets



