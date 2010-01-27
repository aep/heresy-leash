import List
import Data.Maybe
import Control.Monad
import Control.Monad.Instances

import System.Directory
import System.IO
import System( getArgs )
import System.Console.GetOpt

data AppOperation = NoOperation | QueryOperation | SyncOperation
data AppState = AppState{
      dbpath  :: String,
      operation :: AppOperation
    }

data Pkg = Pkg{
      name ::  String,
      desc :: String,
      repo :: String,
      version :: String
    }

argsselect' (state,pkgs) ('S':s) =  (state {operation=SyncOperation} , pkgs  )
argsselect' (state,pkgs) ('Q':s) =  (state {operation=QueryOperation} , pkgs  )

argsselect (state,pkgs) ('-':s)  =  argsselect' (state,pkgs ) s
argsselect (state,pkgs) s        =  (state,pkgs++[s] )

main :: IO ()
main =  do
  args <- getArgs

  let (state,pkgs) = foldl argsselect (AppState {dbpath="/var/lib/pacman/",operation=NoOperation},[]) args

  case  (operation state) of
    SyncOperation  ->  getDirectoryContents'  ((dbpath state)++"sync/")  >>=  mapM_ (\x -> listPkg state ("sync/"++x++"/") >>= mapM_ printPkg)
    QueryOperation ->  listPkg state "local/" >>= mapM_ printPkg
    _ -> putStrLn $ "Usage: m <operation> [...] \n\t-S sync  [packages]\n\t-Q query [packages]"
             

getDirectoryContents' :: String -> IO [String]
getDirectoryContents' path = do
  x <- getDirectoryContents path
  return $ filter (not. (==) "." )  $ filter (not. (==) ".." ) $ sort x
         
data WillBe a = IsNow a | IsAlmost | WillBe

parsePkg :: AppState -> String -> String -> IO (Maybe Pkg)
parsePkg state repo pkd = do
  let path= ((dbpath state)++repo++pkd++"/desc")
  catch (readFile path >>= (\x -> let 
                                       step (a,b,c) "%NAME%"        = (IsAlmost,b,c)
                                       step (IsAlmost,b,c) v        = (IsNow v,b,c)
                                       step (a,b,c) "%DESC%"        = (a,IsAlmost,c)
                                       step (a,IsAlmost,c) v        = (a,IsNow v,c)
                                       step (a,b,c) "%VERSION%"     = (a,b,IsAlmost)
                                       step (a,b,IsAlmost) v        = (a,b,IsNow v)
                                       step (a,b,c) _               = (a,b,c)
                                  in case foldl step (WillBe,WillBe,WillBe) (lines x) of 
                                       (IsNow name, IsNow desc,IsNow version) ->   return $ Just $ Pkg { name=name,desc=desc,repo=repo,version=version }
                                       (_,_,_) -> ( hPutStrLn stderr (path++": invalid desc")  >> return Nothing)
                           )
         )
         (\e ->  hPutStrLn stderr ( show e )  >> return Nothing)
       
listPkg :: AppState -> String -> IO [Pkg]
listPkg state repo = (getDirectoryContents'  ((dbpath state)++repo)  >>= mapM  (parsePkg state repo))  >>= (\x -> return $ catMaybes x )
  
printPkg :: Pkg -> IO ()
printPkg (Pkg name desc repo version) = do 
  putStr repo
  putStr name 
  putStr " " 
  putStrLn version 
  putStr "    " 
  putStrLn desc 


