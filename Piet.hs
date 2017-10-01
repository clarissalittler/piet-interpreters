-- Haskell interpreter for Piet

import Control.Monad.State


{-
data BaseColor = Blue -- 0
               | Magenta -- 1
               | Red -- 2
               | Yellow -- 3
               | Green -- 4
               | Cyan -- 5

-}
data InterpState = {dp :: Int,
                    cc :: Int,
                    board :: Map (Int,Int) Codel,
                    pos :: (Int,Int),
                    stack :: [Int],
                    regionVal :: Int}
                    

type InterpM = StateT InterpState IO 

type BaseColor = Int

type Hue = Int

data Color = C BaseColor Hue

data Codel = BaseColor Color | White | Black

popStack :: InterpM Int
popStack = do
  is <- get
  let st = stack is
  put is{stack = tail st}
  return $ head st

pushStack :: Int -> InterpM ()
pushStack n = do
  is <- get
  let st = stack is
  put is{stack = (n : st)}

binOp :: (Int -> Int -> Int) -> InterpM ()
binOp f = do
  n1 <- popStack
  n2 <- popStack
  pushStack $ f n1 n2

interp :: Command -> InterpM ()
interp Push = undefined
interp Pop = popStack >> return ()
interp Add = binOp (+)
interp Sub = binOp (-)
interp Mult = binOp (*)
interp Div = binOp div
interp Mod = binOp mod
interp Gt = binOp gt
  where gt x y = if y > x then 1 else 0
interp Not = undefined
interp Pointer = undefined
interp Switch = undefined
interp Dup = undefined
interp Roll = undefined
interp InInt = undefined
interp OutInt = undefined
interp InChar = undefined
interp OutChar = undefined

data Command = Push
             | Pop
             | Add
             | Sub
             | Mult
             | Div
             | Mod
             | Not
             | Gt
             | Pointer
             | Switch
             | Dup
             | Roll
             | InInt
             | OutInt
             | InChar
             | OutChar
             deriving (Eq,Show)

-- if the hueDifference is positive then we want to make it negative by subtracting 3, this represents a uniform interface that the hue difference must be 0 1 or 2
hueDifference :: Color -> Color -> Int
hueDifference (C _ h1) (C _ h2) = let d = h2 - h1
                                  in if d < 0
                                     then d + 3
                                     else d

colorDifference :: Color -> Color -> Int
colorDifference (C c1 _) (C c2 _) = let d = c2 - c1
                                    in if d < 0
                                       then d + 5
                                       else d



commandSelect :: Color -> Color -> Command
commandSelect c1 c2 = let hd = hueDifference c1 c2
                          cd = colorDifference c1 c2
                      in case cd of
                           0 -> case hd of
                             0 -> error "We shouldn't have called this with no change"
                             1 -> Push
                             2 -> Pop
                           1 -> case hd of
                             0 -> Add
                             1 -> Sub
                             2 -> Mult
                           2 -> case hd of
                             0 -> Div
                             1 -> Mod
                             2 -> Not
                           3 -> case hd of
                             0 -> Gt
                             1 -> Pointer
                             2 -> Switch
                           4 -> case hd of
                             0 -> Dup
                             1 -> Roll
                             2 -> InInt
                           5 -> case hd of
                             0 -> InChar
                             1 -> OutInt
                             2 -> OutChar
