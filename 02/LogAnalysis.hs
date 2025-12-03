{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg =
    case words msg of
        ("I" : ts : rest) -> LogMessage Info (read ts) (unwords rest)
        ("W" : ts : rest) -> LogMessage Warning (read ts) (unwords rest)
        ("E" : sev : ts : rest) -> LogMessage (Error (read sev) ) (read ts) (unwords rest)
        _ -> Unknown msg

parse :: String -> [LogMessage]
parse file =
    case lines file of
        (x : xs) -> parseMessage x : parse (unlines xs)
        [] -> []

insert :: LogMessage -> MessageTree -> MessageTree

-- Unknown just return the tree
insert (Unknown _) tree = tree

-- For a empty tree append
insert msg Leaf = Node Leaf msg Leaf

-- General node case: handle if the stored node is Unknown or a LogMessage
insert msg@(LogMessage _ ts _) (Node left nodeMsg right) =
  case nodeMsg of
    -- If the node itself is Unknown, just insert into the right subtree
    -- (you can choose a different policy here if you prefer)
    Unknown _ -> Node left nodeMsg (insert msg right)

    -- Normal case: compare timestamps and recurse accordingly
    LogMessage _ nodeTs _ ->
      case compare ts nodeTs of
        LT -> Node (insert msg left) nodeMsg right
        _  -> Node left nodeMsg (insert msg right)

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (s:xs) = insert s (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list =
    case inOrder (build list) of
        [] -> []
        ( (LogMessage (Error sev) _ msg) : xs) ->
            case compare sev 50 of
                GT -> [msg] ++ whatWentWrong xs
                _  -> whatWentWrong xs
        ( _: xs) -> whatWentWrong xs
