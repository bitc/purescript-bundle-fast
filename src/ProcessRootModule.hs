{-# LANGUAGE OverloadedStrings #-}

module ProcessRootModule (
    processRootModule
    ) where

import Control.Monad (foldM)
import Data.Graph
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import System.IO
import qualified Data.Map as M
import qualified Data.Text.IO as T

import ProcessModule

processRootModule ::
  Handle
  -> Namespace
  -> ModuleName
  -> (ModuleName -> IO (Vector Text))
  -> (ModuleName -> IO Text)
  -> IO ()
processRootModule outHandle namespace rootModule loadModule loadForeign = do
    allModules <- processAllModules M.empty namespace rootModule loadModule loadForeign
    let sortedModules = sortModules allModules

    mapM_ writeModule sortedModules

    where
    writeModule (_, moduleLines) =
        mapM_ (T.hPutStrLn outHandle) moduleLines


processAllModules ::
  Map ModuleName (Vector Text, [ModuleName])
  -> Namespace
  -> ModuleName
  -> (ModuleName
  -> IO (Vector Text))
  -> (ModuleName
  -> IO Text)
  -> IO (Map ModuleName (Vector Text, [ModuleName]))
processAllModules collected namespace moduleName loadModule loadForeign =
    case M.lookup moduleName collected of
        Just _ -> return collected
        Nothing -> do
            ls <- loadModule moduleName
            (moduleContents, deps) <- processModule namespace moduleName (loadForeign moduleName) ls
            let newCollected = M.insert moduleName (moduleContents, deps) collected
            foldM processDep newCollected deps

    where
    processDep :: Map ModuleName (Vector Text, [ModuleName]) -> ModuleName -> IO (Map ModuleName (Vector Text, [ModuleName]))
    processDep c m = processAllModules c namespace m loadModule loadForeign

sortModules :: Map ModuleName (Vector Text, [ModuleName]) -> [(ModuleName, Vector Text)]
sortModules modules =
    let sorted = reverse (topSort graph)
    in map readVertex sorted

    where
    nodes = map nodeFunc (M.toList modules)
    nodeFunc (moduleName, (m, moduleDeps)) = (m, moduleName, moduleDeps)
    (graph, nodeFor, _) = graphFromEdges nodes
    readVertex v =
        let (finalModule, moduleName, _) = nodeFor v
        in (moduleName, finalModule)
