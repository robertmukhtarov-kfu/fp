{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Exception.Base
import Control.Monad
import Control.Monad.State
import System.Directory
import System.Environment

getSubdirs :: FilePath -> IO [FilePath]
getSubdirs dir = do
  contentsResult <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case contentsResult of
    Left exception -> 
      (do
        putStrLn $ "Error: " ++ show exception
        pure []
      )
    Right contents -> filterM doesDirectoryExist $ map (\file -> dir ++ "/" ++ file) contents

getSubdirsRecursively :: (MonadState [FilePath] m, MonadIO m) => FilePath -> Int -> m ()
getSubdirsRecursively dir depth = do
  modify (dir :)
  when (depth > 0) $ do
    directories <- liftIO $ getSubdirs dir
    forM_ directories (\dir -> do getSubdirsRecursively dir (depth - 1))

printFileCounts :: FilePath -> IO ()
printFileCounts dir = do
  contentsResult <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case contentsResult of
    Left exception -> 
      do putStrLn $ "Error: couldn't count elements of " ++ dir ++ ". Details: " ++ show exception
    Right contents -> putStrLn $ dir ++ " " ++ show (length contents)

printSizes :: FilePath -> IO ()
printSizes dir = do
  dirSize <- getFileSize dir
  putStrLn $ dir ++ " " ++ show dirSize

printCountAndSize :: FilePath -> Int -> IO ()
printCountAndSize dir depth = do
  dirs <- execStateT (getSubdirsRecursively dir depth) []
  let directories = reverse dirs
  putStrLn "Count:"
  mapM_ printFileCounts directories
  putStrLn "Size:"
  mapM_ printSizes directories

main :: IO ()
main = do
  args <- getArgs
  case length args of
    2 -> printCountAndSize (args !! 0) (read (args !! 1)) 
    1 -> printCountAndSize (args !! 0) (maxBound :: Int)
    0 -> putStrLn "Error: no args provided"
    _ -> putStrLn "Error: too many args"
