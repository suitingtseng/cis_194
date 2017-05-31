{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parseWords :: [String] -> LogMessage
parseWords allStrings@(a:b:xs)
    | a `elem` ["I", "W"] = LogMessage (parseMsgType a 0) (parseMsgTime b) (parseMsgString xs)
    | a == "E" = LogMessage (parseMsgType a (read b)) (parseMsgTime (xs !! 0)) (parseMsgString (drop 1 xs))
    | otherwise = Unknown (unwords ("ill-format line: ":allStrings))
parseWords a = Unknown (unwords ("ill-format line: ":a))

parseMsgType :: String -> Int -> MessageType
parseMsgType a x
    | a == "I" = Info
    | a == "W" = Warning
    | a == "E" = Error x

parseMsgTime :: String -> TimeStamp
parseMsgTime = read . (takeWhile (/= ' '))

parseMsgString :: [String] -> String
parseMsgString = unwords

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert newMsg Leaf = Node Leaf newMsg Leaf
insert newMsg (Node leftTree nodeMsg rightTree)
    | newMsgTime > nodeMsgTime = Node leftTree nodeMsg (insert newMsg rightTree)
    | newMsgTime < nodeMsgTime = Node (insert newMsg leftTree) nodeMsg rightTree
    where (LogMessage _ newMsgTime _) = newMsg
          (LogMessage _ nodeMsgTime _) = nodeMsg

build :: [LogMessage] -> MessageTree
build = foldl (\x y -> insert y x) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = (inOrder leftTree) ++ [msg] ++ (inOrder rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . (filter isError)

getString :: LogMessage -> String
getString (LogMessage _ _ msgString) = msgString

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False
