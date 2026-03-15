{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad (when)
import Data.Aeson
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import GHC.Generics (Generic)
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.Process (createProcess, proc, readProcessWithExitCode, waitForProcess)

data Command
  = Switch (Maybe String) Bool
  | Boot (Maybe String) Bool
  | Update (Maybe String) Bool Bool Bool
  | Deploy (Maybe String) String -- TODO: Figure out how to add remote diffing
  | Diff
  | Info

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "switch" (info switchParser (progDesc "Build configuration and switch to it"))
        <> command "boot" (info bootParser (progDesc "Build configuration and set for next boot"))
        <> command "update" (info updateParser (progDesc "Update configuration flake"))
        <> command "deploy" (info deployParser (progDesc "Deploy configuration to remote host"))
        <> command "diff" (info (pure Diff) (progDesc "Diff system configurations"))
        <> command "info" (info (pure Info) (progDesc "Show system info"))
    )

flakeParser :: Parser (Maybe String)
flakeParser =
  optional $
    strOption
      ( long "flake"
          <> short 'F'
          <> metavar "FLAKE-URL"
          <> help "The flake to operate on. Default is the current directory"
      )

shouldRebuildParser :: Parser Bool
shouldRebuildParser = switch (long "switch" <> help "Rebuild and switch after updating")

commitLockFileParser :: Parser Bool
commitLockFileParser =
  flag
    True
    False
    ( long "no-commit" <> help "Do not commit the updated flake.lock"
    )

diffParser :: Parser Bool
diffParser = switch (long "diff" <> help "Show the diff between the old and new configuration")

hostParser :: Parser String
hostParser = argument str (metavar "HOST")

switchParser, bootParser :: Parser Command
switchParser = Switch <$> flakeParser <*> diffParser
bootParser = Boot <$> flakeParser <*> diffParser

updateParser :: Parser Command
updateParser = Update <$> flakeParser <*> shouldRebuildParser <*> commitLockFileParser <*> diffParser

deployParser :: Parser Command
deployParser = Deploy <$> flakeParser <*> hostParser -- <*> diffParser

run :: Command -> IO ()
run (Switch flake shouldDiff) = do
  nixosRebuild "switch" flake
  when shouldDiff runDiff
run (Boot flake shouldDiff) = do
  nixosRebuild "boot" flake
  when shouldDiff runDiff
run (Update flake shouldRebuild commitLockFile shouldDiff) = do
  let updateArgs = ["flake", "update"] ++ ["--commit-lock-file" | commitLockFile]
  runCmdWithFlake "nix" updateArgs flake
  putStrLn "Update completed"
  when shouldRebuild (run (Switch flake shouldDiff))
run (Deploy flake host) = do
  runCmdWithFlake
    "nixos-rebuild"
    ["switch", "--target-host", host, "--ask-sudo-password"]
    flake
run Diff = runDiff
run Info = runCmd "nixos-version" []

data Generation = Generation
  { generation :: Int,
    date :: String,
    nixosVersion :: String,
    kernelVersion :: String,
    configurationRevision :: String,
    specialisations :: [String],
    current :: Bool
  }
  deriving (Generic, Show)

instance FromJSON Generation

getGenerationsToDiff :: [Generation] -> Maybe (Generation, Generation)
getGenerationsToDiff generations = (,) <$> prevGeneration <*> currentGeneration
  where
    currentGeneration = find current generations
    prevGeneration = currentGeneration >>= (\cgen -> find (\gen -> generation gen == generation cgen - 1) generations)

convertGenerationToPath :: Generation -> String
convertGenerationToPath gen = "/nix/var/nix/profiles/system-" ++ show (generation gen) ++ "-link"

runDiff :: IO ()
runDiff = do
  generationsData <- pack <$> getCmdOutput "nixos-rebuild" ["list-generations", "--json"]
  let generations = either error id (eitherDecodeStrictText generationsData :: Either String [Generation])
  let diffGenerations = fromMaybe (error "Could not find generations to diff") (getGenerationsToDiff generations)
  runCmd
    "nix"
    [ "store",
      "diff-closures",
      convertGenerationToPath $ fst diffGenerations,
      convertGenerationToPath $ snd diffGenerations
    ]

-- | Run "nixos-rebuild --sudo" with the command and flake
nixosRebuild :: String -> Maybe String -> IO ()
nixosRebuild cmd = do
  runCmdWithFlake "nixos-rebuild" [cmd, "--sudo"]

-- | Returns the flake path if it exists. Otherwise, try to look it up from an
-- environment variable and fall back to the current directory
getFlake :: Maybe String -> IO String
getFlake (Just flake) = pure flake
getFlake Nothing = do
  envFlake <- lookupEnv "LEG_FLAKE"
  case envFlake of
    Just flake -> pure flake
    Nothing -> pure "."

-- | Runs the a command with arguments, appending a flake flag with the given
-- flake if it exists
runCmdWithFlake :: String -> [String] -> Maybe String -> IO ()
runCmdWithFlake cmd args flake = do
  flakePath <- getFlake flake
  runCmd cmd (args ++ ["--flake", flakePath])

runCmd :: FilePath -> [String] -> IO ()
runCmd cmd args = do
  (_, _, _, ph) <- createProcess (proc cmd args)
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      putStrLn ("leg: " ++ unwords (cmd : args) ++ " (exit " ++ show code ++ "): failed")
      exitWith exitCode

getCmdOutput :: FilePath -> [String] -> IO String
getCmdOutput cmd args = do
  (exitCode, output, _) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> pure output
    ExitFailure code -> do
      error (unwords (cmd : args) ++ " (exit " ++ show code ++ "): failed")

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Manage NixOs configuration"
            <> header "leg - NixOs configuration manager"
        )
