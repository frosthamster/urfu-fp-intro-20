{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

import System.Directory
import System.IO
import Data.UUID as U
import Data.UUID.V4 as U4
import System.FilePath.Posix
import Data.List
import System.Random

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show, Read)

newtype Id = Id String deriving (Eq, Show, Read)

newtype Title = Title String deriving (Eq, Show, Read)

newtype Deadline = Deadline String deriving (Eq, Show, Read, Ord)

newtype Content = Content String deriving (Eq, Show, Read)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show, Read)


createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectory rootFolder
  return $ TodoList rootFolder


saveTodo :: TodoList -> Todo -> IO ()
saveTodo (TodoList rootDirectory) todo@(Todo {todoId=(Id fileName)}) = 
  withFile (joinPath [rootDirectory, fileName]) WriteMode $ \fd -> do
    hPutStrLn fd $ show $ todo 


addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title content deadline = do
  id <- U4.nextRandom
  let id' = Id $ U.toString id
  saveTodo todoList $ Todo id' title content deadline False
  return $ id'


readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList root) (Id id) = do
  serialized <- readFile $ joinPath [root, id]
  return $ read serialized


showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  todo <- readTodo todoList id
  putStrLn $ show $ todo 


removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList root) (Id id) = do
  removeFile $ joinPath [root, id]


editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit title content deadline) = do
  todo <- readTodo todoList id
  saveTodo todoList $ todo { 
    title = title,
    content = content,
    deadline = deadline
  }


setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  todo <- readTodo todoList id
  saveTodo todoList $ todo { 
    isDone = True
  }


-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList root) = do
  files <- listDirectory root
  todos <- mapM (readTodo todoList) $ map (\id -> Id id) files
  return $ (sortOn (deadline :: Todo -> Deadline) todos)


readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  todos <- readAllTodo todoList
  return $ filter (not . isDone) todos


showTodos :: [Todo] -> IO ()
showTodos todos = do
   mapM_ (putStrLn . show) todos

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = do
  todos <- readAllTodo todoList
  showTodos todos


showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  todos <- readUnfinishedTodo todoList
  showTodos todos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
  secret <- randomRIO(0, 100) :: IO Int
  let step = do
        guess <- readLn
        _ <- putStrLn ("Your number: " ++ show guess)
        case compare guess secret of
          EQ -> putStrLn "Yep, that's the number!"
          LT -> putStrLn "Too small" >> step
          GT -> putStrLn "Too big" >> step
  
  _ <- putStrLn ("Let's go")
  step

-- </Задачи для самостоятельного решения>