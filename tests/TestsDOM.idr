module TestsDOM

import Data.Fin
import Penultimate.Core
import Penultimate.DOM

-- To run this test in a browser/node, we'd need a DOM. We can test pure construction of the grid, or we can polyfill `document` in our test runner script.
export
testDOM : IO ()
testDOM = pure ()
