-- CS4011 MineSweeper Project
-- Jonathan Lepage
-- 17/12/10

module MineSweeper where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Random

g = start minesweepergui

sqr = 16
prefix = "pics/"
postfix = ".png"

data CellState = Uncovered | Covered | Flagged
	deriving (Show, Eq)
	
data CellType = Mine | Safe
	deriving (Show, Eq)
	
data Cell = Cell {
	px :: Int
	, py :: Int
	, ctype :: CellType
	, state :: CellState
	, neighbours :: Int
}

data State = State {
	  topF :: Frame ()
	, sizeX :: Int
	, sizeY :: Int
	, playing :: Bool
	, won :: Bool
	, uncToWin :: Int
	, uncovered :: Int
	, cheatMode :: Bool
	, pics :: [Bitmap ()]
}

minesweepergui :: IO ()
minesweepergui = do
	let pics = map (\ str -> bitmap (prefix ++ str ++ postfix)) ["zero","one","two","three","four","five","six","seven","eight","covered","flag","mine","boom","wrongflag","coveredcheat","wrongflagcheat"]
	size <- readSize
	let (x, y, proba) = size
	--let (x, y, proba) = (16,16,0.2) -- stop asking me
	f <- frame [ text := "Mine Sweeper  -  "++(show x)++" * "++(show y)++" (proba: "++(show proba)++")" ]
	cells <- createMines x y proba
	mines <- computeNeighbours cells x y
	let utw = (x * y) - mines
	putStrLn ("Number of mines: "++(show mines)++" ("++(show utw)++" safe cells to uncover)")
	stv <- varCreate (State f x y True False utw 0 False pics)
	
	bQuit <- button f [text := "Quit", on command := close f]
	bCompFlags <- button f [text := "Compute flags", on command := compFlags f stv cells]
	bOpenSafe <- button f [text := "Open Safe Cells", on command := openSafeNoReturn f stv cells]
	bAutoPlay <- button f [text := "Auto Play", on command := autoPlay f stv cells]
	bCheatMode <- button f [text := "Cheat Mode", on command := cheatModeON f stv cells]
	
	
	set f [
		layout := row 5 [(widget bQuit), (widget bCompFlags), (widget bOpenSafe), (widget bAutoPlay), (widget bCheatMode)]
		, outerSize := sz ((x+2)*sqr + 15) ((y+2)*sqr + 52)
		, on paint := paintCells stv cells
		, on click := checkClick stv cells uncover
		, on clickRight := checkClick stv cells flag
		]
	  
-- READ SIZE
readSize :: IO (Int, Int, Float)
readSize = do
	putStrLn ""
	putStrLn "size X (number of columns)?"
	x <- getLine
	putStrLn "size Y (number of rows)?"
	y <- getLine
	putStrLn "probability of mine ?"
	proba <- getLine
	return ((readInt x), (readInt y), (readFloat proba))
	
readInt :: String -> Int
readInt str = (read str) :: Int

readFloat :: String -> Float
readFloat str = (read str) :: Float

-- GRID GENERATION
createMines :: Int -> Int -> Float -> IO [[Var Cell]]
createMines sizeX sizeY proba = sequence (makeLines proba sizeX sizeY 0)

makeLines :: Float -> Int -> Int -> Int -> [IO [Var Cell]]
makeLines proba sizeX sizeY y =
	if sizeY == y
		then []
		else (sequence (makeLine proba sizeX 0 y)):(makeLines proba sizeX sizeY (y+1))

makeLine :: Float -> Int -> Int -> Int -> [IO (Var Cell)]
makeLine proba sizeX x y =
	if x == sizeX
		then []
		else (createNewCell proba x y):(makeLine proba sizeX (x+1) y)

createNewCell :: Float -> Int -> Int -> IO (Var Cell)
createNewCell proba x y = do
	ctype <- (getRandType proba)
	var <- (varCreate (Cell x y ctype Covered 0))
	return var
	
getRandType :: Float -> IO CellType
getRandType proba = do
	r <- (randomRIO (0.0, 1.0)) :: IO Float
	return (if r < proba
		then Mine
		else Safe)
		
-- COMPUTE NEIGHBOURS
computeNeighbours :: [[Var Cell]] -> Int -> Int -> IO Int
computeNeighbours cells sizeX sizeY = computeNeighboursLines cells cells sizeX sizeY
	
computeNeighboursLines :: [[Var Cell]] -> [[Var Cell]] -> Int -> Int -> IO Int
computeNeighboursLines cells0 [] sizeX sizeY = return 0
computeNeighboursLines cells0 (line:lines) sizeX sizeY = do
	m1 <- computeNeighboursCells cells0 line sizeX sizeY
	m2 <- computeNeighboursLines cells0 lines sizeX sizeY
	return (m1 + m2)
	
computeNeighboursCells :: [[Var Cell]] -> [Var Cell] -> Int -> Int -> IO Int
computeNeighboursCells cells0 [] sizeX sizeY = return 0
computeNeighboursCells cells0 (cell:cells) sizeX sizeY = do
	m1 <- computeNeighboursCell cells0 cell sizeX sizeY
	m2 <- computeNeighboursCells cells0 cells sizeX sizeY
	return (m1 + m2)
	
computeNeighboursCell :: [[Var Cell]] -> Var Cell -> Int -> Int -> IO Int
computeNeighboursCell cells0 clv sizeX sizeY = do
	cl <- varGet clv
	let nbs = getNeighbours cells0 sizeX sizeY (px cl) (py cl)
	m <- sequence (map isMine nbs)
	let count = foldl (\ acc m -> if m then acc+1 else acc) 0 m
	varSet clv cl {neighbours = count}
	return (if (ctype cl) == Mine then 1 else 0)

getNeighbours :: [[Var Cell]] -> Int -> Int -> Int -> Int -> [Var Cell]
getNeighbours cells0 sizeX sizeY x y = do
	let nb = [(-1,-1), (0,-1), (1,-1), (-1,0), (1,0), (-1,1), (0,1), (1,1)]
	let nbc = map (\ (nx,ny) -> (nx+x,ny+y)) nb
	let nbr = filter (\ (cx,cy) -> (cx>=0) && (cx<sizeX) && (cy>=0) && (cy<sizeY)) nbc
	ns <- map (\ (cx,cy) -> getCell cells0 cx cy) nbr
	return ns

getCell :: [[Var Cell]] -> Int -> Int -> Var Cell
getCell cells x y = (cells !! y) !! x

isMine clv = do
	cl <- varGet clv
	return ((ctype cl) == Mine)
	

-- PAINT CELLS
paintCells :: Var State -> [[Var Cell]] -> DC () -> area -> IO ()
paintCells stv cells dc area = do
	paintCellLines cells stv dc
	
paintCellLines :: [[Var Cell]] -> Var State -> DC () -> IO ()
paintCellLines [] stv dc = return ()
paintCellLines (line:lines) stv dc = do
	paintCellLine line stv dc
	paintCellLines lines stv dc
	
paintCellLine :: [Var Cell] -> Var State -> DC () -> IO ()
paintCellLine [] stv dc = return ()
paintCellLine (cell:cells) stv dc = do
	paintCell stv cell dc
	paintCellLine cells stv dc
	
paintCell :: Var State -> Var Cell -> DC () -> IO ()
paintCell stv clv dc = do
	cl <- varGet clv
	st <- varGet stv
	let cellSt = state cl
	let ct = ctype cl
	let cm = cheatMode st
	drawPic dc stv (if playing st
		then if cellSt == Covered
			then if cm && ct == Mine then 14 {- covered cheat -} else 9 -- Covered
			else if cellSt == Flagged
				then if cm && ct == Safe then 15 {- wrong flag cheat -} else 10 -- Flag
				else (neighbours cl) -- Number of neighbours
		else if cellSt == Covered
			then if ct == Mine
				then if (won st)
					then 10 -- Flag
					else 11 -- Mine
				else 9 -- Covered
			else if cellSt == Flagged
				then if ct == Mine
					then 10 -- Flag
					else 13 -- Wrong Flag
				else if ct == Mine
					then 12 -- Exploded
					else (neighbours cl) {- Number of neighbours -} ) (px cl) (py cl)

drawPic :: DC () -> Var State -> Int -> Int -> Int -> IO ()
drawPic dc stv pic x y = do
	st <- varGet stv
	drawBitmap dc ((pics st) !! pic) (pt ((x+1)*sqr) ((y+2)*sqr)) False []

-- MOUSE CLICK
checkClick :: Var State -> [[Var Cell]] -> (Var State -> [[Var Cell]] -> Var Cell -> IO Int) -> Point2 Int -> IO ()
checkClick stv cells func pt = do
	st <- varGet stv
	let (x, y) = getCellFromPixel pt
	if x >= 0 && x < (sizeX st) && y >= 0 && y < (sizeY st)
		then do
			if playing st
				then func stv cells (getCell cells x y)
				else return 0
			repaint (topF st)
		else return ()

getCellFromPixel :: Point2 Int -> (Int, Int)
getCellFromPixel pt = ((div (pointX pt) sqr) - 1, (div (pointY pt) sqr) - 2)

uncover :: Var State -> [[Var Cell]] -> Var Cell -> IO Int
uncover stv cells clv = do
	cl <- varGet clv
	st <- varGet stv
	--putStrLn ("uncovering ("++(show (px cl))++" "++(show (py cl))++")")
	if (state cl == Covered)
		then do
			let ct = ctype cl
			varSet clv cl {state = Uncovered}
			let u = uncovered st
			let utw = uncToWin st
			if ct == Safe
				then if u == utw - 1
					then do
						putStrLn "You won!"
						varSet stv st {playing = False, won = True}
						return 1
					else do
						varSet stv st {uncovered = u + 1}
						if neighbours cl == 0
							then do -- Open the neighborhood
								let nbs = getNeighbours cells (sizeX st) (sizeY st) (px cl) (py cl)
								sequence (map (\ c -> uncover stv cells c) nbs)
								return 1
							else return 1
				else do -- boom!
					putStrLn ("Boom! (You uncovered "++(show u)++"/"++(show utw)++" safe cells)")
					varSet stv st {playing = False}
					return 1
		else return 0
		
flag :: Var State -> [[Var Cell]] -> Var Cell -> IO Int
flag stv cells clv = do
	cl <- varGet clv
	let cst = state cl
	if (cst == Uncovered)
		then return 0
		else do
			varSet clv cl {state = (if cst == Flagged then Covered else Flagged)}
			return 0


-- COMPUTING FLAGS
compFlags :: Frame () -> Var State -> [[Var Cell]] -> IO ()
compFlags f stv cells = do
	st <- varGet stv
	if (playing st)
		then do
			compFlagsLines stv cells cells
			repaint f
		else return ()

compFlagsLines :: Var State -> [[Var Cell]] -> [[Var Cell]] -> IO ()
compFlagsLines stv cells0 [] = return ()
compFlagsLines stv cells0 (line:lines) = do
	compFlagsLine stv cells0 line
	compFlagsLines stv cells0 lines
	
compFlagsLine :: Var State -> [[Var Cell]] -> [Var Cell] -> IO ()
compFlagsLine stv cells0 [] = return ()
compFlagsLine stv cells0 (cell:cells) = do
	compFlagsCell stv cells0 cell
	compFlagsLine stv cells0 cells
	
compFlagsCell :: Var State -> [[Var Cell]] -> Var Cell -> IO ()
compFlagsCell stv cells0 clv = do
	cl <- varGet clv
	if (state cl) == Uncovered
		then do
			st <- varGet stv
			let nbs = getNeighbours cells0 (sizeX st) (sizeY st) (px cl) (py cl)
			ncov <- sequence (map isCovered nbs)
			let nncov = foldl (\ acc b -> if b then acc+1 else acc) 0 ncov
			if nncov > 0 && nncov == (neighbours cl)
				then flagAround stv cells0 clv
				else return ()
		else return ()

isCovered :: Var Cell -> IO Bool
isCovered clv = do
	cl <- varGet clv
	return (not ((state cl) == Uncovered))
	
flagAround :: Var State -> [[Var Cell]] -> Var Cell -> IO ()
flagAround stv cells0 clv = do
	st <- varGet stv
	cl <- varGet clv
	let nbs = getNeighbours cells0 (sizeX st) (sizeY st) (px cl) (py cl)
	sequence (map flagON nbs)
	return ()

flagON :: Var Cell -> IO ()
flagON clv = do
	cl <- varGet clv
	if (state cl) == Covered
		then varSet clv cl {state = Flagged}
		else return ()
	
-- OPEN SAFE CELLS
openSafeNoReturn :: Frame () -> Var State -> [[Var Cell]] -> IO ()
openSafeNoReturn f stv cells = do
	openSafe f stv cells
	repaint f
	return ()

openSafe :: Frame () -> Var State -> [[Var Cell]] -> IO Int
openSafe f stv cells = do
	st <- varGet stv
	if (playing st)
		then openSafeLines stv cells cells
		else return 0

openSafeLines :: Var State -> [[Var Cell]] -> [[Var Cell]] -> IO Int
openSafeLines stv cells0 [] = return 0
openSafeLines stv cells0 (line:lines) = do
	op1 <- openSafeLine stv cells0 line
	op2 <- openSafeLines stv cells0 lines
	return (op1 + op2)
	
openSafeLine :: Var State -> [[Var Cell]] -> [Var Cell] -> IO Int
openSafeLine stv cells0 [] = return 0
openSafeLine stv cells0 (cell:cells) = do
	op1 <- openSafeCell stv cells0 cell
	op2 <- openSafeLine stv cells0 cells
	return (op1 + op2)
	
openSafeCell :: Var State -> [[Var Cell]] -> Var Cell -> IO Int
openSafeCell stv cells0 clv = do
	cl <- varGet clv
	if (state cl) == Uncovered
		then do
			let (nei, x, y) = (neighbours cl, px cl, py cl)
			st <- varGet stv
			let nbs = getNeighbours cells0 (sizeX st) (sizeY st) x y
			nflg <- sequence (map isFlagged nbs)
			let nnflg = foldl (\ acc b -> if b then acc+1 else acc) 0 nflg
			if nnflg > 0 && nnflg == nei
				then openAround stv cells0 clv
				else if nnflg > nei
					then do
						putStrLn ("Warning! You have "++(show nnflg)++" flags around cell ("++(show x)++","++(show y)++") which has "++(show nei)++" mine"++(if nei > 1 then "s" else "")++" around!")
						return 0
					else return 0
		else return 0

isFlagged :: Var Cell -> IO Bool
isFlagged clv = do
	cl <- varGet clv
	return ((state cl) == Flagged)

openAround :: Var State -> [[Var Cell]] -> Var Cell -> IO Int
openAround stv cells0 clv = do
	st <- varGet stv
	cl <- varGet clv
	let nbs = getNeighbours cells0 (sizeX st) (sizeY st) (px cl) (py cl)
	--putStrLn ("opening neighbours of ("++(show (px cl))++" "++(show (py cl))++")")
	opening <- sequence (map (\ cell -> uncover stv cells0 cell) nbs)
	let nbOpened = foldl (\ acc n -> if n == 1 then acc + 1 else acc) 0 opening
	--putStrLn ("opened: "++(show nbOpened))
	return nbOpened

-- AUTO PLAY
autoPlay :: Frame () -> Var State -> [[Var Cell]] -> IO ()
autoPlay f stv cells = do
	st <- varGet stv
	if (playing st)
		then do
			compFlags f stv cells
			opened <- openSafe f stv cells
			if (opened > 0)
				then autoPlay f stv cells
				else return ()
		else return ()

-- CHEAT MODE
cheatModeON :: Frame () -> Var State -> [[Var Cell]] -> IO ()
cheatModeON f stv cells = do
	st <- varGet stv
	varSet stv st {cheatMode = True}
	repaint f



