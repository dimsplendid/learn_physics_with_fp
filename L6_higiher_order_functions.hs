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

-- Predicate-Based Higher-Order Function

-- a -> Bool, where a is any valid Haskell type.

greaterThanOrEq7 :: Int -> Bool
greaterThanOrEq7 n = if n >= 7 then True else False

-- filter :: (a -> Bool) -> [a] -> [a]
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile (a -> Bool) -> [a] -> [a]

-- any (a -> Bool) -> [a] -> bool
-- all (a -> Bool) -> [a] -> bool

-- A list comprehension can also do the work of filter
-- [x | x <- [1,2,3,4,5], mod x 2 == 0]
--                      ^^^^^^^^^^^^^^ guard

-- Numeriacl Integration
-- An intergrator is stateful.

integral :: R -> Integration
integral dt f a b -- 這寫法真的很潮:D
    = sum [f t * dt | t <- [a+dt/2, a+3*dt/2 .. b-dt/2]]

antiderivative :: R -> AntiDerivative
antiderivative dt v0 a t = v0 + integral dt a 0 t

velFromAcc ::  
    R                       -- dt
 -> Velocity                -- initial velocity
 -> (Time -> Acceloration)  -- acceleration function
 -> (Time -> Velocity)      -- velocity function
velFromAcc dt v0 a t = antiderivative dt v0 a t

posFromVel ::
    R                       -- dt
 -> Position                -- initial position
 -> (Time -> Velocity)      -- velocity function
 -> (Time -> Position)      -- position function
posFromVel = antiderivative

integralN :: Int -> Integration
integralN n f a b
    = let dt = (b - a) / fromIntegral n
      in integral dt f a b