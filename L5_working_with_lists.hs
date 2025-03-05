physicists :: [String] 
physicists = ["Einstein","Newton","Maxwell"]

type R = Double
velocities :: [R]
velocities = [0, -9.8,-19.6,-29.4]

moreVelocities :: [R]
moreVelocities = [-39.2, -49.0]

shortWords :: [String]
shortWords = ["am", "I", "to"]

ns :: [Int]
ns = [0..10]

funcs :: [R -> R]
funcs = [(\x -> x), (\x -> x^2), (\x -> x^3)]

ts :: [R]
ts = [0, 0.1..6]

yRock30 :: R -> R
yRock30 t = 30 * t - 4.9 * t^2

xs :: [R]
xs = [yRock30 t | t <- ts]

sndItem :: [a] -> a
sndItem ys = case ys of
    [] -> error "Empty list has no second element"
    (x:xs) -> if null xs
        then error "1-item list has no second element"
        else head xs

sndItem2 :: [a] -> a
sndItem2 [] = error "Empty list has no second element"
sndItem2 (x:xs) = if null xs
    then error "1-item list has no second element"
    else head xs

sndItem3 :: [a] -> a
sndItem3 ys = case ys of
    [] -> error "Empty list has no second element"
    (x:[]) -> error "1-item list has no second element"
    (x:z:_) -> z

sndItem4 :: [a] -> a
sndItem4 [] = error "Empty list has no second element"
sndItem4 (x:[]) = error "1-item list has no second element"
sndItem4 (x:z:_) = z