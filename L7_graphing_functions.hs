-- Functions with the type R -> R are functions that can be plotted on a graph.
-- using gnuplot to show diagram in this chapter, the installation in windows
-- is a little tricky.

import Data.List
import Types
import Graphics.Gnuplot.Simple

square :: R -> R
square x = x**2

plot1 :: IO()
plot1 = plotFunc [] [-3, -2.99 .. 3] square

