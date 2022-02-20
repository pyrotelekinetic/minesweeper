{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.Except
import Control.Applicative
import System.Random
import Data.Array.IArray
import Data.List
import Graphics.Vty

data Cell
	= Mined
	| Hidden
	| Flagged
	| Shown Int
	deriving (Eq, Show)

type Field = Array (Int, Int) Cell
type Coord = (Int, Int)

won :: Field -> Bool
won fa = not $ elem Hidden fa

renderCell :: Bool -> Cell -> Image
renderCell False c = case c of
	Shown n -> string (defAttr `withForeColor` blue) $ "[" ++ show n ++ "]"
	Flagged -> string (defAttr `withForeColor` red) $ "[F]"
	_ -> string (defAttr `withForeColor` white) "[·]"
renderCell True c = case c of
	Shown n -> string (defAttr `withForeColor` green) $ "[" ++ show n ++ "]"
	Flagged -> string (defAttr `withForeColor` green) $ "[F]"
	_ -> string (defAttr `withForeColor` green) "[·]"

plant :: Eq a => Int -> a -> [a] -> Maybe [a]
plant _ _ [] = Nothing
plant 0 a' (a : as)
	| a' == a = Nothing
	| otherwise = pure $ a' : as
plant n a' (a : as) = do
	as' <- plant (n - 1) a' as
	pure $ a : as'

scatter :: Eq a => Int -> a -> [a] -> IO [a]
scatter 0 _ as = pure as
scatter n a' as = do
	r <- randomRIO (0, length as)
	maybe (scatter n a' as) (scatter (n - 1) a') $ plant r a' as

touching :: Coord -> Field -> Int
touching (x, y) fa = go $ map (fa !)
	[(i, j)
		| i <- [(x - 1), x, (x + 1)]
		, j <- [(y - 1), y, (y + 1)]
		, (i, j) /= (x, y)
		, inBounds (i, j) fa
	]
		where
		go :: [Cell] -> Int
		go [] = 0
		go (Mined : cs) = go cs + 1
		go (_ : cs) = go cs

adjacent :: Field -> Coord -> [Coord]
adjacent fa (x, y) =
	[(i, j)
		| i <- [(x - 1), x, (x + 1)]
		, j <- [(y - 1), y, (y + 1)]
		, (i, j) /= (x, y)
		, inBounds (i, j) fa
	]

inBounds :: Coord -> Field -> Bool
inBounds (x, y) fa = case bounds fa of
	((lx, ly), (hx, hy)) | x >= lx && x <= hx && y >= ly && y <= hy -> True
	_ -> False

getRows :: Field -> [[Cell]]
getRows fa = [[fa ! (i, j) | i <- [0..xb]] | j <- [0..yb]]
	where (_, (xb, yb)) = bounds fa

getRowsCursor :: Field -> Coord -> [[(Bool, Cell)]]
getRowsCursor fa cursorPos = [[((i, j) == cursorPos, fa ! (i, j)) | i <- [0..xb]] | j <- [0..yb]]
  where (_, (xb, yb)) = bounds fa


flag :: Field -> Coord -> Field
flag fa xy = case fa ! xy of
	Flagged -> fa // [(xy, Hidden)]
	Shown _ -> fa
	_ -> fa // [(xy, Flagged)]

-- Idea: add cells to list as they are uncovered, then check against list to prevent looping
uncover :: Field -> Coord -> Field
uncover fa xy = fa // (map (\a -> (a, Shown $ touching a fa)) $ snd $ go ([], []) fa xy)
	where
	go :: ([Coord], [Coord]) -> Field -> Coord -> ([Coord], [Coord])
	go (xys, xys') fa xy
		| notElem xy xys = case fa ! xy of
			Mined -> (xy : xys, xys') -- TODO
			Hidden
				| touching xy fa == 0 -> foldl' (flip go fa) (xy : xys, xy : xys') $ adjacent fa xy
				| otherwise -> (xy : xys, xy : xys')
			_ -> (xy : xys, xys')
		| otherwise = (xys, xys')

draw :: Field -> Coord -> Picture
draw fa cpos = picForImage $ vertCat $ map (horizCat . map (uncurry renderCell)) $ getRowsCursor fa cpos

drawWin :: Field -> Picture
drawWin fa = picForImage $ vertCat $ (map (horizCat . map (renderCell False)) $ getRows fa) ++ map (string (defAttr `withForeColor` blue)) ["You Won!", "Press q to quit."]

drawWin' :: Field -> String
drawWin' fa = show $ vertCat $ map (horizCat . map (renderCell False)) $ getRows fa

renderField :: Vty -> Field -> Coord -> IO ()
renderField vty fa (x, y)
	| won fa = do
		update vty $ drawWin fa
		waitQuit
	| otherwise = do
	update vty $ draw fa (x, y)
	e <- nextEvent vty
	case e of
		EvKey (KChar 'q') _ -> shutdown vty
		EvKey (KChar 'f') _ -> renderField vty (flag fa (x, y)) (x, y)
		EvKey (KChar 'd') _ -> renderField vty (uncover fa (x, y)) (x, y)
		EvKey (KChar 'h') _ -> renderField vty fa (x - 1, y)
		EvKey (KChar 'j') _ -> renderField vty fa (x, y + 1)
		EvKey (KChar 'k') _ -> renderField vty fa (x, y - 1)
		EvKey (KChar 'l') _ -> renderField vty fa (x + 1, y)
		_ -> renderField vty fa (x, y) --don't accidentally break things by giving uncaught input
	where
	waitQuit = do
		e <- nextEvent vty
		case e of
			EvKey (KChar 'q') _ -> shutdown vty
			_ -> waitQuit

genField :: (Int, Int) -> Int -> IO Field
genField (x, y) m = do
	as <- scatter m Mined (replicate ((x + 1) * (y + 1)) Hidden)
	pure $ listArray ((0, 0), (x, y)) as

main = do
	cfg <- standardIOConfig
	vty <- mkVty cfg
	field <- genField (5, 5) 3
	renderField vty field (0, 0)

testField :: Field
testField = listArray ((0, 0), (2, 2)) $ replicate 9 Hidden
