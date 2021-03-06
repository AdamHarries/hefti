{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Environment
  ( BuildException (..),
    BuildEnv (..),
    prepareEnvironment,
    makeRule,
    CmdLineArgs (..),
    opts,
    copyToDir,
    AbsDir(..),
    AbsFile(..),
    RelFile(..),
  )
where

import           Control.Exception
import           Data.Semigroup      ((<>))
import           Data.Text           as TE
import           Options.Applicative
import           Path
import           Path.IO
import qualified System.Directory    as SD
import qualified System.FilePath     as SF
import           Type.Reflection

data BuildException
  = NoSourceDir
  | NoSourceFiles
  | CannotFindMuseScore
  | CouldNotGetMSCX
  | CannotWriteMSCZ
  | IllegalPDFFile TE.Text
  | IllegalMSCXFilePath TE.Text
  deriving (Show, Typeable)

instance Exception BuildException

data BuildEnv = BuildEnv
  { source_d     :: AbsDir,
    book_d       :: AbsDir,
    build_d      :: AbsDir,
    parts_d      :: AbsDir,
    tex_d        :: AbsDir,
    pdf_d        :: AbsDir,
    release_name :: String,
    mscore_path  :: AbsFile,
    notex        :: Bool,
    nobooks      :: Bool
  }
  deriving (Show)

prepareEnvironment :: CmdLineArgs -> IO BuildEnv
prepareEnvironment options = do
  source_ex <- doesDirExist $ arg_source_d options
  if source_ex
    then putStrLn "Found source directory"
    else throw NoSourceDir
  absMScorePath <-
    (findExecutable $ arg_mscore_path options) >>= \apath ->
      case apath of
        Just p -> putStrLn ("Found musescore executable at " ++ (show p)) >> pure p
        Nothing -> throw CannotFindMuseScore
  ensureDir $ arg_build_d options -- create build
  ensureDir $ arg_book_d options -- and book
  let parts_d = (arg_build_d options) </> [reldir|parts|]
  let tex_d = (arg_build_d options) </> [reldir|tex|]
  let pdf_d = (arg_build_d options) </> [reldir|pdf|]
  ensureDir $ parts_d
  ensureDir $ tex_d
  ensureDir $ pdf_d
  pure
    ( BuildEnv
        (arg_source_d options)
        (arg_book_d options)
        (arg_build_d options)
        parts_d
        tex_d
        pdf_d
        (arg_release_name options)
        absMScorePath
        (arg_notex options)
        (arg_nobooks options)
    )

-- Given a source file, a target file, and an effectful function that will create the latter from
-- the former, check whether the target exists, or if it is newer than the source, and if either
-- are false then run the function to create the output.
makeRule :: AbsFile -> AbsFile -> (AbsFile -> AbsFile -> IO ()) -> IO ()
makeRule source target f = do
  targetExists <- doesFileExist target
  if targetExists
    then do
      sourceTime <- getModificationTime source
      targetTime <- getModificationTime target
      if sourceTime > targetTime then do
        putStrLn $ "Creating target:\n\t" ++ (show target) ++ "\n from newer source:\n\t" ++ (show source)
        f source target
      else
        putStrLn $ "Target:\n\t" ++ (show target) ++ "\n is newer than source:\n\t" ++ (show source) ++ "\n Skipping..."
    else do
      putStrLn $ "Target:\n\t" ++ (show target) ++ "\n does not exist, creating from source:\n\t" ++ (show source)
      f source target

type AbsDir = Path Abs Dir
type AbsFile = Path Abs File
type RelFile = Path Rel File

data CmdLineArgs = CmdLineArgs
  { arg_source_d     :: AbsDir,
    arg_book_d       :: AbsDir,
    arg_build_d      :: AbsDir,
    arg_release_name :: String,
    arg_mscore_path  :: RelFile,
    arg_notex        :: Bool,
    arg_nobooks      :: Bool
  }
  deriving (Show)

-- Append a filepath to an absolute directory.
-- this bypasses some of the restrictions from the Path library, as we turn the "safe" Path
-- into an unsafe FilePath so that we can normalise indirection. Because of this, we might not
-- end up with a valid path at the end, so we return a "maybe" absolute directory.
unsafeAppendPath :: AbsDir -> FilePath -> Maybe AbsDir
unsafeAppendPath base app =
  parseAbsDir $ SF.normalise $ normaliseIndirection $ (toFilePath base) SF.</> app


-- Normalise path indirection, i.e.:
-- normaliseIndirection "/home/bar/../bar/foo/../foo" = "/home/bar/foo"
normaliseIndirection :: FilePath -> FilePath
normaliseIndirection fp =
  let elements = SF.splitPath fp
   in normAdj (Prelude.head elements) (Prelude.tail elements)
  where
    normAdj :: FilePath -> [FilePath] -> FilePath
    normAdj fp [] = fp
    normAdj fp (x : []) = fp SF.</> x
    normAdj fp (up : lower : xs) =
      if lower == "../"
        then normAdj fp xs
        else normAdj (fp SF.</> up) (lower : xs)

tidyFilename :: TE.Text -> TE.Text
tidyFilename = TE.concatMap replace
  where
    replace :: Char -> Text
    replace ' '  = "_"
    replace '('  = ""
    replace ')'  = ""
    replace ','  = ""
    replace '\'' = ""
    replace '\"' = ""
    replace c    = singleton c

parseCmdLineArgs :: AbsDir -> Parser CmdLineArgs
parseCmdLineArgs base = CmdLineArgs
  <$> option
    parseDirectory
    ( long "source"
        <> short 's'
        <> value ( base </> [reldir|src|])
        <> metavar "SRC"
        <> help
          "Source directory for musescore (mscx, mscz) files (default - `$(pwd)/src`)"
    )
  <*> option
    parseDirectory
    ( long "book"
        <> short 'o'
        <> value ( base </> [reldir|books|])
        <> metavar "BOOK"
        <> help
          "Output folder to write (finished) books (default - `$(pwd)/books`)"
    )
  <*> option
    parseDirectory
    ( long "build"
        <> short 'b'
        <> value ( base </> [reldir|build|])
        <> metavar "BUILD"
        <> help
          "Temporary folder for storing intermediate files (default - `$(pwd)/build`)"
    )
  <*> strOption
    ( long "release"
        <> short 'r'
        <> value "unnamed"
        <> metavar "RELEASE"
        <> help "Version name for the generated books (default - `unnamed`)"
    )
  <*> option
    parseFile
    ( long "musescore"
        <> short 'm'
        <> value [relfile|mscore-portable|]
        <> metavar "MUSESCORE"
        <> help
          "Path to musescore command line program (default -`mscore-portable`)"
    )
  <*> switch
    ( long "notex"
      <> short 'n'
      <> help "Disable latex for building books, and use pdftk to concatenate them instead"
    )
  <*> switch
    ( long "nobooks"
      <> short 'c'
      <> help "Disable concatenating parts into books, instead just copy all part pdfs to book directory"
    )
  where
    parseDirectory :: ReadM AbsDir
    parseDirectory = eitherReader $ eitherAppendPath base where
      eitherAppendPath :: AbsDir -> FilePath -> Either String AbsDir
      eitherAppendPath base app = case unsafeAppendPath base app of
        Just p  -> Right p
        Nothing -> Left ("Failed to parse path: " ++ app)

    parseFile :: ReadM RelFile
    parseFile =
      eitherReader $ \path ->
        case parseRelFile path of
          Just p  -> Right p
          Nothing -> Left ("Failed to parse relative path: " ++ path)

opts :: AbsDir -> ParserInfo CmdLineArgs
opts base =
  info
    ((parseCmdLineArgs base) <**> helper)
    ( fullDesc
        <> progDesc "Generate pdf \"books\" of charts from individual musescore files in src"
        <> header "Hefti - an arranging tool for song scores into player books"
    )

copyToDir :: AbsFile -> AbsDir -> IO AbsFile
copyToDir f d = let newf =  d </> (filename f) in
  SD.copyFile (toFilePath f) (toFilePath newf) >> (pure newf)

