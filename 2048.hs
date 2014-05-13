import System.Time
import Control.Applicative

data State = Clear Int | GameOver Int | OnPlay Int
type Field = [[Int]]

showField (Field xs) = concat (map show' xs) ++ horizon where
	show' xs = horizon ++ vert ++ concat (map f xs) ++ "|\n" ++ vert
	horizon = replicate 20 '-' ++ "\n"
	vert = concat (replicate 4 "|    ") ++ "|\n"
	f x = "|" ++ replicate (4-length (show x)) ' ' ++ show x

instance Show State where
	show state =header ++ "\n" ++ "SCORE : " ++ show (getScore state) where
		header = case state of
			(Clear _) -> replicate 30 '*' ++ "CLEAR" ++ replicate 30 '*' 
			(GameOver _) -> replicate 20 '*' ++ "GAME OVER..." ++ replicate 20 '*'
			(OnPlay _) -> []

getScore (Clear score) = score
getScore (OnPlay score) = score
getScore (GameOver score) = score

initialize a b = divide 4 $ replicate i 0 ++ [2] ++ replicate (j-i-1) 0 ++ [2] ++ replicate (16-max i j - 1) 0 where
	i1 = mod a 16 `min` mod b 16
	i2 = mod a 16 `max` mod b 16
	(i,j) = if i1 == i2 then (i1,i2+1) else (i1,i2)
	divide n [] = []
	divide n xs = take n xs : divide n (drop n xs)

data Direction = Up | Down | Right | Left

getCommad = do
	comm <- getChar >> getChar >> getChar
	return case comm of
		'A' -> Up
		'B' -> Down
		'C' -> Right
		'D' -> Left
		 _  -> getCommand

main = do
	time <- toUTCTime <$> getClockTime
	let initField = initialize (ctSec time) (ctMin time)
	result <- game initField (OnPlay 0)
	putStrLn $ show result
	writeFile "score.txt" $ show (getScore result)

game :: Field -> State -> IO State
game fd (OnPlay score) = do
	putStrLn $ showField fd
	command <- getCommnad
	let (field,state') = next command fd score
	game field state'
game _ result = return result

next :: Direction -> Field -> Int -> (Field,State)
next key fd score = if any (==2048) $ concat fd
	then (fd,Clear score)
	else if cannotMove fd
		then (fd,GameOver score)
		else move key fd

move :: Direction -> Field -> (Field,Int)
move Up = map moveVert [0,1,2,3]
move Down = map moveVert [3,2,1,0]
move Left = map moveHorizon
move Right = map moveHorizon

moveVert direction xs = case direction of
	Up -> map moveVert'
	Down ->  

moveHorizon direction xs = case direction of
	Left  -> moveHorizon' xs
	Right -> reverse $ moveHorizon' (reverse xs)

moveHorizon' xs = fd' ++ replicate (4-length fd') 0 where
	fd' = joint ([],0)$ filter (/=0) xs
	joint (result,pt) [] = (result,pt)
	joint (result,pt) [x] = (result++[x],pt)
	joint (result,pt) (x:y:ys) = if x == y
		then joint (result**[2*x],pt+2*x) ys
		else joint (result:[x],pt) (y:ys)

cannotMove :: Field -> Bool
cannotMove fd = not $ foldl (check fd) False [0..15]

--i:縦, j:横
check :: Field -> Bool -> Int -> Bool
check fd result a = result || any check' poses  where
	(i,j) = toIndex a
	poses = filter inField [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
	check' p = fd `at` p == fd `at` (i,j) || fd `at` p == 0

toIndex a = (div a 4, mod a 4)
fd `at` (i,j) = (fd !! i) !! j
inField (i,j) = 0 <= i && i < 4 && 0 <= j && j < 4
