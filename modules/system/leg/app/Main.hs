{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (when)
import Options.Applicative
import System.Environment (lookupEnv)
import System.Process (callProcess)

data Command
  = Switch {flakePath :: Maybe String}
  | Boot {flakePath :: Maybe String}
  | Update {shouldRebuild :: Bool, flakePath :: Maybe String}
  | Info

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "switch" (info switchParser (progDesc "Build configuration and switch to it"))
        <> command "boot" (info bootParser (progDesc "Build configuration and set for next boot"))
        <> command "update" (info updateParser (progDesc "Update configuration flake"))
        <> command "info" (info (pure Info) (progDesc "Show system info"))
    )

flakeParser :: Parser (Maybe String)
flakeParser =
  optional $
    strOption
      ( long "flake"
          <> metavar "FLAKE-URL"
          <> help "The flake to operate on. Default is the current directory"
      )

shouldRebuildParser :: Parser Bool
shouldRebuildParser =
  flag
    True
    False
    ( long "no-switch" <> help "Do not rebuild and switch after updating"
    )

switchParser, bootParser :: Parser Command
switchParser = Switch <$> flakeParser
bootParser = Boot <$> flakeParser

updateParser :: Parser Command
updateParser = Update <$> shouldRebuildParser <*> flakeParser

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
run (Update shouldRebuild flake) = do
  runCmdWithFlake "nix" ["flake", "update"] flake
  when shouldRebuild (nixosRebuild "switch" flake)
run Info = do
  runCmd "nixos-version" []
  flakePath <- getFlake Nothing
  putStrLn flakePath

nixosRebuild :: String -> Maybe String -> IO ()
nixosRebuild cmd = runCmdWithFlake "sudo" ["nixos-rebuild", cmd]

getFlake :: Maybe String -> IO String
getFlake (Just flake) = pure flake
getFlake Nothing = do
  envFlake <- lookupEnv "LEG_FLAKE"
  case envFlake of
    Just flake -> pure flake
    Nothing -> pure "."

runCmdWithFlake :: String -> [String] -> Maybe String -> IO ()
runCmdWithFlake cmd args flake = do
  flakePath <- getFlake flake
  runCmd cmd (args ++ ["--flake", flakePath])

runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
  putStrLn $ "→ " ++ unwords (cmd : args)
  callProcess cmd args
