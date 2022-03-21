import GHC.Conc (ThreadStatus(ThreadDied))
-- Frame, Throw, Hits for each throw
data Game = Game Int Throw [(Int,Int,Int)]
instance Show Game where
    show (Game frame throw hits) = show frame ++ " " ++ show throw ++ " " ++ show hits 
data Throw =  One | Two | Three deriving Show

-- Scenarios
-- 1st roll of frame, not a stike -> inc throw append to hits inner list
-- 1st roll of frame, strike -> inc frame append [10] to hits
-- 2nd roll of frame -> inc frame, reset throw to 1, append to hits inner list
roll:: Game -> Int -> Game
roll (Game f One hits) 10 = Game (f+1) One (hits++[(10,0,0)])
roll (Game f One hits) pins = Game f Two (hits++[(pins,0,0)])
roll (Game f Two hits) pins = Game (f+1) One (init hits++[(fst3 (last hits),pins,0)])
roll _ _ = undefined

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

score:: Game -> Int
score _ = 0