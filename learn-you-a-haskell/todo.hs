-- todo
import Control.Exception
import Data.List
import System.Directory
import System.Environment
import System.IO


main :: IO ()
main = do
    args <- getArgs
    if null args then putStrLn emptyError else dispatch (head args) (tail args)


dispatch :: String -> [String] -> IO ()
dispatch "add"    = add
dispatch "bump"   = bump
dispatch "view"   = view
dispatch "remove" = remove
dispatch command  = doesntExist command


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _                    = putStrLn $ errorMessage 2


bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks    = lines contents
        taskToMove   = todoTasks !! (read numberString :: Int)
        newTodoItems = unlines $ taskToMove : (delete (taskToMove) todoTasks)
    commit fileName newTodoItems
bump _                        = putStrLn $ errorMessage 2


commit :: FilePath -> String -> IO ()
commit fileName content =
    bracketOnError (openTempFile "./io-files/" "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle content
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)


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
    commit fileName newTodoItems
remove _                        = putStrLn $ errorMessage 2


view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks     = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStrLn "\nThese are your TO-DO items:"
    putStrLn $ unlines numberedTasks
view _          = putStrLn $ errorMessage 1

