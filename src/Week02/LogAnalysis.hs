module Week02.LogAnalysis where

import Data.List (foldl')

data MessageType =
    Info
  | Warning
  | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage =
    LogMessage MessageType TimeStamp String
  | Unknown String
  deriving (Show, Eq)

data MessageTree =
    Leaf
  | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage message =
  case words message of
    "I":ts:xs          -> makeLogMessage Info ts xs
    "W":ts:xs          -> makeLogMessage Warning ts xs
    "E":severity:ts:xs -> makeLogMessage (Error $ read severity) ts xs
    xs                   -> Unknown (unwords xs)

makeLogMessage :: MessageType -> String -> [String] -> LogMessage
makeLogMessage messageType ts remainingWords =
  LogMessage messageType (read ts) (unwords remainingWords)

parse :: String -> [LogMessage]
parse text = parseMessage <$> lines text

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg         Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _)
       (Node left nodeMsg@(LogMessage _ nodeTs _) right)
  | ts < nodeTs         = Node (insert msg left) nodeMsg right
  | otherwise           = Node left              nodeMsg (insert msg right)
insert _ _              = error "An unknown snuck into the tree!"

build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ (msg : inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . filter isHighSeverityError . inOrder . build

isHighSeverityError :: LogMessage -> Bool
isHighSeverityError (LogMessage (Error severity) _ _) = severity > 50
isHighSeverityError _                                 = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ message) = message
extractMessage (Unknown message)        = message

-- | @testParse n f@ tests the log file parser by running it on the
--   first @n@ lines of file @f@.
testParse :: Int -> FilePath -> IO [LogMessage]
testParse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong f@ tests the log file parser and warning
--   message extractor by running them on the log file @f@.
testWhatWentWrong :: FilePath -> IO [String]
testWhatWentWrong file = whatWentWrong . parse <$> readFile file
