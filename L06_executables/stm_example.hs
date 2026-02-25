{-# LANGUAGE BlockArguments #-}

import Control.Concurrent     (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad          (forever)
import System.Random.Stateful (initStdGen, newAtomicGenM, uniformRM)

------------------------------------- GATE -------------------------------------

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do
    tv <- newTVar 0
    return (MkGate n tv)

passGate :: Gate -> IO ()
passGate (MkGate _ tv) = atomically do
    n_left <- readTVar tv
    check (n_left > 0)
    writeTVar tv (n_left - 1)

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
    atomically (writeTVar tv n)
    atomically do
        n_left <- readTVar tv
        check (n_left == 0)

------------------------------------ GROUP -------------------------------------

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically do
    g1 <- newGate n
    g2 <- newGate n
    tv <- newTVar (n, g1, g2)
    return (MkGroup n tv)

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup _ tv) = atomically do
    (n_left, g1, g2) <- readTVar tv
    check (n_left > 0)
    writeTVar tv (n_left - 1, g1, g2)
    return (g1, g2)

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
    (n_left, g1, g2) <- readTVar tv
    check (n_left == 0)
    new_g1 <- newGate n
    new_g2 <- newGate n
    writeTVar tv (n, new_g1, new_g2)
    return (g1, g2)

----------------------------------- WORKERS ------------------------------------

worker :: Group -> IO () -> IO ThreadId
worker group action = forkIO do
    gen <- initStdGen >>= newAtomicGenM
    forever do
        (in_gate, out_gate) <- joinGroup group
        passGate in_gate >> action >> passGate out_gate
        uniformRM (1, 1000000) gen >>= threadDelay

elf :: Group -> Int -> IO ThreadId
elf group iden = worker group do
    putStr ("Elf " ++ show iden ++ " meeting in the study\n")

reindeer :: Group -> Int -> IO ThreadId
reindeer group iden = worker group do
    putStr ("Reindeer " ++ show iden ++ " delivering toys\n")

------------------------------------ SANTA -------------------------------------

main :: IO ()
main = do
    elf_group <- newGroup 3
    sequence_ [ elf elf_group n | n <- [1..10] ]
    rein_group <- newGroup 9
    sequence_ [ reindeer rein_group n | n <- [1..9] ]
    forever do
        putStr "----------\n"
        (task, (in_gate, out_gate)) <- atomically (orElse
            (chooseGroup rein_group "deliver toys")
            (chooseGroup elf_group "meet in my study"))
        putStrLn ("Ho! Ho! Ho! let’s " ++ task)
        operateGate in_gate
        operateGate out_gate
  where
    chooseGroup :: Group -> String -> STM (String, (Gate,Gate))
    chooseGroup gp task = do
        gates <- awaitGroup gp
        return (task, gates)
