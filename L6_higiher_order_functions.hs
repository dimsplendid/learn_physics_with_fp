import Types

springForce5500 :: R -> R
springForce5500 x = -5500 * x

springForce :: R -> R -> R -- right associated,  equal to R -> (R -> R)
springForce k x = -k * x

-- Imperative programming languages provide ways to write loops.
-- Functional programming languages have a different way to express iteration. 
-- The most popular way to achieve iteration among functional programmers 
-- is to write recursive functions.
-- -> Lists instead of loops
-- In this book, we use prelude `iterate` function to do iteration.
-- iterate f = [x, f x, f (f x), f (f (f x)), ...]

-- How to use iterate to build fabonacci sequence?
-- a_n = a_{n-1} + a_{n-2}
-- a_0 = 0, a_1 = 1