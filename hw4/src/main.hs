import qualified EX1
import qualified EX3
import qualified EX4




main = do
    let testData1 = [[1, 2, 3, 5], [130, 578, 1, 3], [], [1]]
    mapM_ (EX1.putFun1 EX1.fun1 EX1.fun1') testData1

    let testData2 = [1, 20, 58, 3]
    mapM_ (EX1.putFun2 EX1.fun2 EX1.fun2') testData2

    let testData3 = [[False, True, False],  [False, True, False, False, True]]
    mapM_ (EX3.putXor EX3.xor EX3.xor') testData3

    let testData4 = [[1, 2, 3, 5], [130, 578, 1, 3], [], [1]]
    mapM_ (EX3.putMap map EX3.map') testData4

    let testData5 = [5, 80, 27, 87]
    mapM_ EX4.putSieve testData5
