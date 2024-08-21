stepFuncton :: Double -> Double
stepFuncton x = if x <= 0 then 0 else 1

type R = Double

type Time         = R
type TimeInterval = R
type Position     = R
type Velocity     = R
type Acceloration = Double

type PositionFunction = Time -> Position
type VelocityFunction = Time -> Velocity
type AccelorationFunction = Time -> Acceloration

type Derivative = (R -> R) -> (R -> R)

avgVelocity1 :: Time -> Time -> PositionFunction -> Velocity
avgVelocity1 t0 t1 x = (x t1 - x t0) / (t1 - t0)

avgVelocity2 :: Time -> TimeInterval -> PositionFunction -> Velocity
avgVelocity2 t dt x = (x (t + dt/2) - x (t - dt/2)) / dt