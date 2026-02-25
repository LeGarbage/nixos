module Main (main) where

import Control.Monad (when)
import Options.Applicative
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.Process (createProcess, proc, waitForProcess)

-- TODO: Add gc, edit, and rollback commands
data Command
  = Switch (Maybe String)
  | Boot (Maybe String)
  | Update (Maybe String) Bool Bool
  | Deploy (Maybe String) String
  | Info

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "switch" (info switchParser (progDesc "Build configuration and switch to it"))
        <> command "boot" (info bootParser (progDesc "Build configuration and set for next boot"))
        <> command "update" (info updateParser (progDesc "Update configuration flake"))
        <> command "deploy" (info deployParser (progDesc "Deploy configuration to remote host"))
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

hostParser :: Parser String
hostParser = argument str (metavar "HOST")

switchParser, bootParser :: Parser Command
switchParser = Switch <$> flakeParser
bootParser = Boot <$> flakeParser

updateParser :: Parser Command
updateParser = Update <$> flakeParser <*> shouldRebuildParser <*> commitLockFileParser

deployParser :: Parser Command
deployParser = Deploy <$> flakeParser <*> hostParser

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

run :: Command -> IO ()
run (Switch flake) = nixosRebuild "switch" flake
run (Boot flake) = nixosRebuild "boot" flake
run (Update flake shouldRebuild commitLockFile) = do
  let updateArgs = ["flake", "update"] ++ ["--commit-lock-file" | commitLockFile]
  runCmdWithFlake "nix" updateArgs flake
  putStrLn "Update completed"
  when shouldRebuild (nixosRebuild "switch" flake)
run (Deploy flake host) =
  runCmdWithFlake
    "nixos-rebuild"
    ["switch", "--target-host", host, "--ask-sudo-password"]
    flake
run Info = do
  _ <- runCmd "nixos-version" []
  pure ()

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

-- | Creates a new process to run the specified command with the given
-- arguments, and wait for it to finish. If the command returns a non-zero
-- exit code, an exception is raised.
runCmd :: FilePath -> [String] -> IO ()
runCmd cmd args = do
  putStrLn $ "Executing " ++ unwords (cmd : args)
  (_, _, _, ph) <- createProcess (proc cmd args)
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      putStrLn ("leg: " ++ unwords (cmd : args) ++ " (exit " ++ show code ++ "): failed")
      exitWith exitCode
