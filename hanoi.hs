
import Control.Applicative
import Control.Monad
import Control.Monad.Writer

data Column = L | M | R
              deriving (Show, Read, Eq, Ord)

data Move = Move {getFrom :: Column,
                  getTo :: Column}
            deriving (Show, Read, Eq, Ord)

data PMove = PMove {getValue :: Int,
                    getMove :: Move}
             deriving (Show, Read, Eq, Ord)

data Board = Board {getLeft :: Rings,
                    getMiddle :: Rings,
                    getRight :: Rings}
             deriving (Show, Read, Eq)

type Rings = [Int]

get :: Column -> Board -> Rings
get L = getLeft
get M = getMiddle
get R = getRight

mutate :: Column -> (Rings -> Rings) -> Board -> Board
mutate L f b = b {getLeft = f $ getLeft b}
mutate M f b = b {getMiddle = f $ getMiddle b}
mutate R f b = b {getRight = f $ getRight b}

other :: Column -> Column -> Column
other L M = R
other L R = M
other M R = L
other x y
    | x == y = error "Matching values in other"
    | otherwise = other y x

instance Enum Column where
    fromEnum x = case x of
                   L -> 0
                   M -> 1
                   R -> 2
    toEnum x = case x of
                 0 -> L
                 1 -> M
                 2 -> R
                 _ -> error "Invalid number in toEnum"

instance Bounded Column where
    minBound = L
    maxBound = R

doMove :: Board -> PMove -> Writer [Move] Board
doMove b (PMove v m)
    | v `elem` get (getTo m) b = return b -- Already in destination position
    | not $ v `elem` get (getFrom m) b = return b -- Not in source position
    | v == (head $ get (getFrom m) b) = do
  tell [m]
  return . mutate (getFrom m) tail . mutate (getTo m) (v :) $ b
    | otherwise = do
  let firstMove = head . until ((== v) . (!! 1)) tail $ get (getFrom m) b
      otherCol = other (getFrom m) (getTo m)
  b' <- doMove b (PMove firstMove $ Move (getFrom m) otherCol)
  b'' <- doMove b' (PMove v m)
  doMove b'' (PMove firstMove $ Move otherCol (getTo m))

-- Call this function
tower :: Int -> [Move]
tower n = let board = Board [1 .. n] [] []
          in snd . runWriter . doMove board . PMove n $ Move L M
