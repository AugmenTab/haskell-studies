-- todo
import Control.Exception
import Data.List
import System.Environment
import System.Directory
import System.IO


main :: IO ()
main = do
    args <- getArgs
    if null args then putStrLn emptyError else dispatch (head args) (tail args)


dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
-- TODO dispatch "bump"   = bump
dispatch "view"   = view
dispatch "remove" = remove
dispatch command  = doesntExist command


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _                    = putStrLn $ errorMessage 2


doesntExist :: String -> [String] -> IO ()
doesntExist command _ = 
    putStrLn $ "The " ++ command ++ " command doesn't exist."


emptyError :: String
emptyError = "You have not provided any arguments."


errorMessage :: Int -> String
errorMessage n = "This command takes exactly " ++ show n ++ " argument(s)."


remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks     = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    let number = read numberString :: Int
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "./io-files/" "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
remove _                        = putStrLn $ errorMessage 2


view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks     = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStrLn $ unlines numberedTasks
view _          = putStrLn $ errorMessage 1

