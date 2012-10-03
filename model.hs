module Model where
import Control.Monad.State

-- []+-.,><
data BF = Inc | Dec | Loop [BF] | LeftShift | RightShift | Read | Write deriving Show
data BFOut = Add Int| SetZero | LoopOut [BFOut] Int | Shift Int | ReadOut | WriteOut deriving Show


-- state is count of loops
transform :: [BF] -> [BFOut]
transform bf = optimize $ evalState (transform1 bf)  0

transform1 :: [BF] -> State Int [BFOut]
transform1 bfs = mapM transformItem bfs

transformItem :: BF -> State Int BFOut
transformItem Inc = return $ Add 1
transformItem Dec = return $ Add (-1)
transformItem (Loop bfs) = do label <- get
                              modify (+1)
                              bfsOut <- mapM transformItem bfs
                              return $ LoopOut bfsOut label
transformItem LeftShift = return $ Shift (-1)
transformItem RightShift = return $ Shift 1
transformItem Read = return ReadOut
transformItem Write = return WriteOut

optimize :: [BFOut] -> [BFOut]
optimize [] = []
optimize (Add a:Add b:as) = optimize ((Add ((a+b) `mod` 256)):as)
optimize (Add 0:as) = optimize as
optimize (Shift a:Shift b:as) = optimize ((Shift (a+b)):as)
optimize (Shift 0:as) = optimize as
optimize ((LoopOut bfs n):as) = case optimize bfs of
                                  [Add _] -> SetZero:optimize as
                                  bfs2 -> (LoopOut bfs2 n):optimize as
optimize (a:as) = a:optimize as

