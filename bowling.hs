-- Frame, Throw, Hits for each throw
data Game = Game Int Throw [(Int,Int)] Bool 
instance Show Game where
    show (Game frame throw hits strikeBonusRoll) = show frame ++ " " ++ show throw ++ " " ++ show hits ++ showBonusRoll strikeBonusRoll
        where 
            showBonusRoll True = " Bonus roll from strike"
            showBonusRoll False = ""
data Throw =  One | Two  deriving Show
data StrikeOrSpare = Strike | Spare | Neither
instance Show StrikeOrSpare where
    show Strike = "Strike"
    show Spare = "Spare"
    show Neither = "Neither"

initialGame = Game 1 One [] False 

-- Scenarios
-- 1st roll of frame, not a stike -> inc throw append to hits inner list
-- 1st roll of frame, strike -> inc frame append [10] to hits
-- 2nd roll of frame -> inc frame, reset throw to 1, append to hits inner list
-- 10th frame spare -> inc frame, set throw to 2, append to hits inner list
-- 10th frame, 2nd throw -> frame = 12, append to hits inner list
roll:: Game -> Int -> Game
roll g@(Game 12 _ _ _) _ = g
roll (Game 11 _ hits True) pins = Game 12 One (appendToLastPair hits pins) False 
roll (Game 10 Two hits _) pins
    | fst (last hits) + pins == 10 = Game 11 One (appendToLastPair hits pins) True 
    | otherwise = Game 12 One (appendToLastPair hits pins) False 
roll (Game f One hits _) 10 = Game (f+1) One (hits++[(10,0)]) False 
roll (Game f One hits _) pins = Game f Two (hits++[(pins,0)]) False 
roll (Game f Two hits _) pins = Game (f+1) One (appendToLastPair hits pins) False 

appendToLastPair:: [(Int,Int)] -> Int -> [(Int,Int)]
appendToLastPair pairs x = init pairs++[(fst (last pairs),x)]


score :: Game -> Int
score (Game _ _ hits _) = sum $ zipWith (*) (flattenHits hits) modifiers
    where modifiers = modifiersFromStrikeSpares $ repeatSpareNeithers $ map strikeOrSpare hits

strikeOrSpare :: (Int , Int) -> StrikeOrSpare
strikeOrSpare (a, b)
    | a == 10 = Strike
    | a+b == 10 = Spare
    | otherwise = Neither

repeatSpareNeithers :: [StrikeOrSpare] -> [StrikeOrSpare]
repeatSpareNeithers (Strike:ss) = Strike : repeatSpareNeithers ss
repeatSpareNeithers (Spare:ss) = Neither : Spare : repeatSpareNeithers ss
repeatSpareNeithers (Neither:ss) = Neither : Neither : repeatSpareNeithers ss
repeatSpareNeithers [] = []

modifiersFromStrikeSpares ss = map (1 +) (convert ss [0,0])
    where
        convert (Strike:ss) is = convert ss (init is ++ [1 + last is] ++ [1])
        convert (Spare:ss) is = convert ss (init is ++ [1 + last is] ++ [0])
        convert (Neither:ss) is = convert ss (init is ++ [0 + last is] ++ [0])
        convert [] is = is

flattenHits :: [(Int,Int )] -> [Int ]
flattenHits ((10,b):hs) = 10 : flattenHits hs
flattenHits ((a,b):hs) = a : b :flattenHits hs
flattenHits [] = []