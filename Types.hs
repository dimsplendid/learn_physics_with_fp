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