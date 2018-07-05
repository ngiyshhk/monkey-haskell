import           Lexer
import           Token
import           Text.Printf
import           System.Posix.User
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

main :: IO ()
main = do
  u <- getLoginName
  putStrLn $ printf "Hello %s! This is the Monkey programming language!" u
  putStrLn "Feel free to type in commands"
  l <- getLine
  print $ runState toTokens l
