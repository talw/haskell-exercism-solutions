module Robot (robotName, mkRobot, resetName)
where

import System.Random
import Data.IORef
import Control.Monad.State

robotName :: IORef String -> IO String
robotName = readIORef

mkRobot :: IO (IORef String)
mkRobot = newIORef . calcRobotName =<< newStdGen

resetName :: IORef String -> IO ()
resetName ref = writeIORef ref . calcRobotName =<< newStdGen

calcRobotName :: RandomGen g => g -> String
calcRobotName = evalState $ liftM2 (++)
    (replicateM 2 $ state $ randomR ('A','Z')) $
     replicateM 3 $ state $ randomR ('0', '9')

{-calcRobotName n = evalState runMe $ mkStdGen n-}
    {-where runMe = do-}
            {-letter1 <- state $ randomR ('A','Z')-}
            {-letter2 <- state $ randomR ('A','Z')-}
            {-digit1 <- state $ randomR ('0','9')-}
            {-digit2 <- state $ randomR ('0','9')-}
            {-digit3 <- state $ randomR ('0','9')-}
            {-return [letter1,letter2,digit1,digit2,digit3]-}
