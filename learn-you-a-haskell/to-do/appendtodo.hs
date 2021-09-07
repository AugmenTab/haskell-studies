-- appendtodo
import System.IO


main :: IO ()
main = do
    todoItem <- getLine
    appendFile "io-files/todo.txt" (todoItem ++ "\n")
