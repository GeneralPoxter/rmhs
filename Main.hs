import Control.Exception
import Control.Monad
import Data.List
import Options.Applicative
import System.Directory
import System.Directory.Internal.Prelude
import System.FilePath

data Options = Options
  { optVersion :: Bool,
    optVerbose :: Bool,
    optProtect :: Bool,
    optUnprotect :: Bool,
    optKeep :: Bool,
    optForce :: Bool,
    optPrompt :: Bool,
    optFiles :: [String]
  }

optParser :: Parser Options
optParser =
  Options
    <$> switch
      ( long "version"
          <> short 'V'
          <> help "Outputs version and exits"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Print out files that are being processed"
      )
    <*> switch
      ( long "protect"
          <> short 'p'
          <> help "Protects specified files"
      )
    <*> switch
      ( long "unprotect"
          <> short 'u'
          <> help "Removes protection for specified files"
      )
    <*> switch
      ( long "keep"
          <> short 'k'
          <> help "Keeps protection for files that are removed (overriden if parent directory is removed)"
      )
    <*> switch
      ( long "force"
          <> short 'f'
          <> help "Removes all specified files, ignoring protection"
      )
    <*> switch
      ( long "interactive"
          <> short 'i'
          <> help "Prompts before removing protected files"
      )
    <*> many (argument str (metavar "FILES/DIRECTORIES..."))

main :: IO ()
main = execParser totalParser >>= runProg
  where
    totalParser =
      info
        (optParser <**> helper)
        ( fullDesc
            <> progDesc "Removes files and directories with protection safeguards. Run with '-h' for more info."
            <> header "rmhs - A safe rm command"
        )

data ObjectType = File | Directory | Neither

data Object = Object ObjectType FilePath

classify :: FilePath -> IO Object
classify f = do
  isFile <- doesFileExist f
  if isFile
    then return $ Object File f
    else do
      isDirectory <- doesDirectoryExist f
      if isDirectory
        then return $ Object Directory f
        else return $ Object Neither f

-- Classifies collection of FilePaths into collection of Objects, onto which function is mapped
runOnFiles :: Foldable t => Options -> (Options -> Object -> IO ()) -> t FilePath -> IO ()
runOnFiles opts m = mapM_ (classify >=> m opts)

createFile :: FilePath -> IO ()
createFile name = writeFile name ""

isShield :: FilePath -> Bool
isShield f = isPrefixOf ".shield" $ takeFileName f

filterListDirectory :: FilePath -> IO [FilePath]
filterListDirectory f = filter (not . isShield) <$> listDirectory f

prettyPrintError :: IOException -> String
prettyPrintError e
  | isPermissionError e = "Permission denied"
  | isDoesNotExistError e = "File/directory does not exist"
  | otherwise = show e

printIfVerbose :: Options -> String -> IO ()
printIfVerbose opts s
  | optVerbose opts = putStrLn s
  | otherwise = return ()

genName :: Object -> FilePath
genName (Object File f)
  | isShield f = "/"
  | otherwise = takeDirectory f </> ".shield-" ++ takeFileName f
genName (Object Directory f) = f </> ".shield"
genName (Object Neither f) = ""

gracefulRemove :: Options -> (FilePath -> IO ()) -> FilePath -> IO ()
gracefulRemove opts m f = do
  result <- try (m f) :: IO (Either IOException ())
  case result of
    Left e -> putStrLn $ concat ["Could not remove ", f, ": ", prettyPrintError e]
    Right v -> printIfVerbose opts $ "Removed " ++ f

doesProtectionExist :: Object -> IO Bool
doesProtectionExist = doesFileExist . genName

protect :: Options -> Object -> IO ()
protect opts (Object t f)
  | name == "" = putStrLn $ concat ["Error: ", f, " is neither a file nor a directory"]
  | name == "/" = putStrLn "Error: cannot protect .shield files"
  | otherwise = do
    createFile name
    printIfVerbose opts $ "Protected " ++ f
  where
    name = genName $ Object t f

unprotect :: Options -> Object -> IO ()
unprotect opts o = do
  isProtected <- doesProtectionExist o
  if isProtected
    then gracefulRemove opts removeFile $ genName o
    else putStrLn $ concat ["Error: ", f, " is not protected"]
  where
    Object _ f = o

forceRemove :: Options -> Object -> IO ()
forceRemove opts (Object File f) = do
  isProtected <- doesProtectionExist $ Object File f
  when (not (optKeep opts) && isProtected) $ gracefulRemove opts removeFile $ genName $ Object File f
  gracefulRemove opts removeFile f
forceRemove opts (Object Directory f) = do
  filterListDirectory f >>= runOnFiles opts forceRemove . fmap (combine f)
  listing <- filterListDirectory f
  when (null listing) $ do
    listDirectory f >>= runOnFiles opts forceRemove . fmap (combine f)
    gracefulRemove opts removeDirectory f
forceRemove _ (Object Neither f) = putStrLn $ concat ["Error: ", f, " is neither a file nor a directory"]

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

promptRemove :: Options -> Object -> IO ()
promptRemove opts o = do
  isProtected <- doesProtectionExist o
  if isProtected
    then case t of
      File -> do
        res <- prompt $ concat ["Remove protected file ", f, "? [Y/n] "]
        when (res == "Y") $ forceRemove opts o
      Directory -> do
        res <- prompt $ concat ["Remove protected directory ", f, "? [Y/n] "]
        when (res == "Y") $ do
          filterListDirectory f >>= runOnFiles opts promptRemove . fmap (combine f)
          listing <- filterListDirectory f
          when (null listing) $ forceRemove opts o
      Neither -> putStrLn $ concat ["Error: ", f, " is neither a file nor a directory"]
    else forceRemove opts o
  where
    Object t f = o

safeRemove :: Options -> Object -> IO ()
safeRemove opts o = do
  isProtected <- doesProtectionExist o
  if isProtected
    then do printIfVerbose opts $ concat ["Protected: ", f, " was not removed"]
    else forceRemove opts o
  where
    Object _ f = o

runProg :: Options -> IO ()
runProg opts
  | optVersion opts = putStrLn "0.1"
  | optProtect opts = runOnFiles opts protect files
  | optUnprotect opts = runOnFiles opts unprotect files
  | optForce opts = runOnFiles opts forceRemove files
  | optPrompt opts = runOnFiles opts promptRemove files
  | otherwise = runOnFiles opts safeRemove files
  where
    files = optFiles opts