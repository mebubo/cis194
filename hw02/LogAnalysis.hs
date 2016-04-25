module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ss =
    case words ss of
      "I":t:rest -> LogMessage Info (read t) (unwords rest)
      "W":t:rest -> LogMessage Warning (read t) (unwords rest)
      "E":l:t:rest -> LogMessage (Error (read l)) (read t) (unwords rest)
      _ -> Unknown ss

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m1@(LogMessage _ t1 _) (Node left m2@(LogMessage _ t2 _) right) =
    if t1 <= t2 then Node (insert m1 left) m2 right
                else Node left m2 (insert m2 right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right)= inOrder left ++ [m] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . inOrder . build . filter atLeast50
    where
          atLeast50 (LogMessage (Error l) _ _) = l >= 50
          atLeast50 _ = False
          extractMessage (LogMessage _ _ str)  = str
