module IdT (countEntries, localExample) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM_, liftM, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Reader (Reader, ask, local)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

-- runWriterT $ countEntries "path"
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntries newName

myName :: String -> Reader String String
myName step = do
    name <- ask
    return (step ++ ", I am " ++ name)

-- runReader localExample "string"
localExample :: Reader String (String, String, String)
localExample = do
    a <- myName "First"
    b <- local (++ "dy") (myName "Second")
    c <- myName "Third"
    return (a, b, c)