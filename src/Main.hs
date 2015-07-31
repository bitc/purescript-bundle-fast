{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Vector (Vector)
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import ProcessModule
import ProcessRootModule

defaultNamespace :: Namespace
defaultNamespace = "PS"

main :: IO ()
main = do
    processRootModule stdout defaultNamespace "Main" loadModule loadForeign

    where
    loadModule :: ModuleName -> IO (Vector Text)
    loadModule moduleName = do
        contents <- T.readFile $ "output/" ++ T.unpack moduleName ++ "/index.js"
        return $ V.fromList (T.lines contents)

    loadForeign :: ModuleName -> IO Text
    loadForeign moduleName = T.readFile $ "output/" ++ T.unpack moduleName ++ "/foreign.js"
