import Control.Exception
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
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

createFile :: FilePath -> IO ()
createFile name = writeFile name ""

isHsield :: FilePath -> Bool
isHsield f = isPrefixOf ".hsield" $ takeFileName f

filterListDirectory :: FilePath -> IO [FilePath]
filterListDirectory f = filter (not . isHsield) <$> listDirectory f

prettyPrintError :: IOException -> String
prettyPrintError e
  | isPermissionError e = "Permission denied"
  | isDoesNotExistError e = "File/directory does not exist"
  | otherwise = show e

printIfVerbose :: Options -> String -> IO ()
printIfVerbose opts s
  | optVerbose opts = putStrLn s
  | otherwise = return ()

genName :: FilePath -> IO FilePath
genName f = do
  isFile <- doesFileExist f
  if isFile
    then
      if isHsield f
        then return "/"
        else return $ takeDirectory f </> ".hsield-" ++ takeFileName f
    else do
      isDirectory <- doesDirectoryExist f
      if isDirectory
        then return $ f </> ".hsield"
        else return ""

gracefulRemove :: Options -> (FilePath -> IO ()) -> FilePath -> IO ()
gracefulRemove opts m f = do
  result <- try (m f) :: IO (Either IOException ())
  case result of
    Left e -> putStrLn $ concat ["Could not remove ", f, ": ", prettyPrintError e]
    Right v -> printIfVerbose opts $ "Removed " ++ f

doesProtectionExist :: FilePath -> IO Bool
doesProtectionExist f = do
  name <- genName f
  doesFileExist name

protect :: Options -> FilePath -> IO ()
protect opts f = do
  name <- genName f
  if name == ""
    then putStrLn $ concat ["Error: ", f, " is neither a file nor a directory"]
    else
      if name == "/"
        then putStrLn "Error: cannot protect .hsield files"
        else do
          createFile name
          printIfVerbose opts $ "Protected " ++ f

unprotect :: Options -> FilePath -> IO ()
unprotect opts f = do
  isProtected <- doesProtectionExist f
  if isProtected
    then do
      name <- genName f
      gracefulRemove opts removeFile name
    else putStrLn $ concat ["Error: ", f, " is not protected"]

forceRemove :: Options -> FilePath -> IO ()
forceRemove opts f = do
  isFile <- doesFileExist f
  if isFile
    then do
      isProtected <- doesProtectionExist f
      when (not (optKeep opts) && isProtected) $ do
        name <- genName f
        gracefulRemove opts removeFile name
      gracefulRemove opts removeFile f
    else do
      isDirectory <- doesDirectoryExist f
      if isDirectory
        then do
          listing <- filterListDirectory f
          mapM_ (forceRemove opts . combine f) listing
          listing <- filterListDirectory f
          when (null listing) $ do
            listing <- listDirectory f
            mapM_ (forceRemove opts . combine f) listing
            gracefulRemove opts removeDirectory f
        else putStrLn $ concat ["Error: ", f, " is neither a file nor a directory"]

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

promptRemove :: Options -> FilePath -> IO ()
promptRemove opts f = do
  isProtected <- doesProtectionExist f
  if isProtected
    then do
      isFile <- doesFileExist f
      if isFile
        then do
          res <- prompt $ concat ["Remove protected file ", f, "? [Y/n] "]
          when (res == "Y") $ forceRemove opts f
        else do
          isDirectory <- doesDirectoryExist f
          if isDirectory
            then do
              res <- prompt $ concat ["Remove protected directory ", f, "? [Y/n] "]
              when (res == "Y") $ do
                listing <- filterListDirectory f
                mapM_ (promptRemove opts . combine f) listing
                listing <- filterListDirectory f
                when (null listing) $ forceRemove opts f
            else putStrLn $ concat ["Error: ", f, " is neither a file nor a directory"]
    else forceRemove opts f

safeRemove :: Options -> FilePath -> IO ()
safeRemove opts f = do
  isProtected <- doesProtectionExist f
  if isProtected
    then do printIfVerbose opts $ concat ["Protected: ", f, " was not removed"]
    else forceRemove opts f

runProg :: Options -> IO ()
runProg opts
  | optVersion opts = putStrLn "0.1"
  | optProtect opts = mapM_ (protect opts) $ optFiles opts
  | optUnprotect opts = mapM_ (unprotect opts) $ optFiles opts
  | optForce opts = mapM_ (forceRemove opts) $ optFiles opts
  | optPrompt opts = mapM_ (promptRemove opts) $ optFiles opts
  | otherwise = mapM_ (safeRemove opts) $ optFiles opts