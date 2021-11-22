{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE PatternSynonyms #-}

import Control.Monad.Except

type Cell = Bool

pattern Mine = True
pattern Empty = False

type Field = [[Cell]]

type Error = String

raise :: String -> Either Error a
raise = throwError

initList :: a -> Int -> Either Error [a]
initList _ n | n < 0 = raise "error: cannot make negative length list"
initList x n = pure $ go x n []
	where
		go :: a -> Int -> [a] -> [a]
		go x 0 xs = xs
		go x n xs = x : go x (n - 1) xs

initField :: (Int, Int) -> Either Error Field
initField (0, _) = raise "error: x cannot be 0"
initField (_, 0) = raise "error: y cannot be 0"
initField (x, y) = do
	x' <- initList Empty x
	y' <- initList x' y
	pure y'

main = print "noerr"
