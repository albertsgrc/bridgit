module Queue where

data Queue a = Queue [a] [a]

queue :: Queue a
queue = Queue [] []

buildQueue :: [a] -> Queue a
buildQueue = foldl push queue

push :: Queue a -> a -> Queue a
push (Queue l1 l2) x = Queue l1 (x:l2)

pop :: Queue a -> Queue a
pop (Queue [] x)     = Queue l [] where (_:l) = reverse x
pop (Queue (_:xs) y) = Queue xs y

top :: Queue a -> a
top (Queue [] x)    = last x
top (Queue (x:_) _) = x

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False