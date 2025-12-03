{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage msg =
    case words msg of
     ("I":ts:rest) -> LogMessage Info (read ts) (unwords rest)
     ("W":ts:rest) -> LogMessage Warning (read ts) (unwords rest)
     ("E":sev:ts:rest) -> LogMessage (Error (read sev)) (read ts) (unwords rest)
     _ -> Unknown msg

parse :: String -> [LogMessage]
parse messages =
    case lines messages of
    (first:rest) -> parseMessage(first) : parse(unlines rest)
    _ -> []
