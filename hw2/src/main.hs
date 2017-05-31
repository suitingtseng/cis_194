import LogAnalysis
import Log

main :: IO ()
main = do
    logMessages <- (testParse parse 25 "error.log")
    mapM_ putStrLn (map show logMessages)

    let tree = build logMessages
    putStrLn (show tree)

    let sortedMsgs = inOrder tree
    mapM_ putStrLn (map show sortedMsgs)

    errorMsgs <- testWhatWentWrong parse whatWentWrong "error.log"
    putStrLn (unlines errorMsgs)
