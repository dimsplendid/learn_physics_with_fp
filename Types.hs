module Types where

type R = Double

type Time         = R
type TimeInterval = R
type Position     = R
type Velocity     = R
type Acceloration = R

type PositionFunction = Time -> Position
type VelocityFunction = Time -> Velocity
type AccelorationFunction = Time -> Acceloration

type Derivative = (R -> R) -> (R -> R)

-- L6 Higher-order Functions

type Integration = (R -> R) -- funciton
                -> R        -- lower limit
                -> R        -- upper limit
                -> R        -- result