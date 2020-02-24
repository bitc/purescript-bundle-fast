{-# LANGUAGE OverloadedStrings #-}

module ProcessModule
    ( Namespace
    , ModuleName
    , processModule
    ) where

import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Text as T
import qualified Data.Vector as V

type ModuleName = Text

type Namespace = Text

processModule :: Namespace -> ModuleName -> IO Text -> Vector Text -> IO (Vector Text, [ModuleName])
processModule namespace modulename loadForeign ls = do
    let importSectionLoc = findImportSection ls
        exportSectionIndex = findExportSectionIndex ls

        (importLines, reqModules, needsForeign) = createImportSection namespace modulename ls importSectionLoc
        exportLines = createExportSection ls exportSectionIndex

    foreignModule <- if needsForeign then processForeign namespace modulename loadForeign
                                     else return V.empty

    let output = V.concat $
            case importSectionLoc of
                Nothing -> case exportSectionIndex of
                    Nothing ->
                        [ foreignModule
                        , V.singleton jsModuleHeader
                        , ls
                        , V.singleton (jsModuleFooter namespace modulename)
                        ]
                    Just ei ->
                        [ foreignModule
                        , V.singleton jsModuleHeader
                        , V.slice 0 ei ls
                        , exportLines
                        , V.singleton (jsModuleFooter namespace modulename)
                        ]
                Just (ii, ic) -> case exportSectionIndex of
                    Nothing ->
                        [ foreignModule
                        , V.singleton jsModuleHeader
                        , V.slice 0 ii ls
                        , importLines
                        , V.drop (ii+ic) ls
                        , V.singleton (jsModuleFooter namespace modulename)
                        ]
                    Just ei ->
                        [ foreignModule
                        , V.singleton jsModuleHeader
                        , V.slice 0 ii ls
                        , importLines
                        , V.slice (ii+ic) (ei - (ii + ic)) ls
                        , exportLines
                        , V.singleton (jsModuleFooter namespace modulename)
                        ]

    return (output, reqModules)


-- Searches for a string formatted as:
--
-- var Data_Maybe = require("../Data.Maybe");
--
-- In this case will return: Just ("Data_Maybe", "Data.Maybe")
--
parseImportStatement :: Text -> Maybe (Text, ModuleName)
parseImportStatement l =
    case T.stripPrefix "var " l of
        Nothing -> Nothing
        Just r1 -> case T.breakOn " = require(" r1 of
            (_, "") -> Nothing
            (varName, r2) -> case T.breakOn ");" (T.drop (T.length " = require(") r2) of
                (_, "") -> Nothing
                (quotedModName, _) -> case () of
                    _ | isSurroundedBy '"' quotedModName -> Just (varName, removeRelativePath (removeQuotes quotedModName))
                    _ | isSurroundedBy '\'' quotedModName -> Just (varName, removeRelativePath (removeQuotes quotedModName))
                    _ | otherwise -> Nothing

    where
    isSurroundedBy :: Char -> Text -> Bool
    isSurroundedBy c t =
        T.head t == c && T.last t == c

    removeQuotes :: Text -> Text
    removeQuotes =
        T.tail . T.init

    removeRelativePath :: Text -> Text
    removeRelativePath t =
         fromMaybe t (T.stripPrefix "../" t >>= T.stripSuffix "/index.js")

formatImport :: Namespace -> (Text, Text) -> Text
formatImport namespace (varName, modName) =
    T.concat ["var ", varName, " = ", namespace, "[\"", modName, "\"];"]

-- Searches for a string formatted like the following examples:
--
--     ">>=": $greater$greater$eq,
--     bind: bind,
--     showOrdering: showOrdering
--
parseExportStatement :: Text -> Maybe (Text, Text)
parseExportStatement line =
    case parseFieldName (T.stripStart line) of
        Nothing -> Nothing
        Just (fieldName, rest) -> case parseFieldValue rest of
            Nothing -> Nothing
            Just fieldValue -> Just (fieldName, fieldValue)

    where
    parseFieldName :: Text -> Maybe (Text, Text)
    parseFieldName l =
        case T.head l of
            '\"' -> case T.breakOn "\"" (T.tail l) of
                (_, "") -> Nothing
                (fieldName, rest) -> Just (fieldName, T.stripStart (T.tail rest))
            _ -> case T.breakOn ":" l of
                (_, "") -> Nothing
                ("", _) -> Nothing
                (fieldName, rest) -> Just (T.stripEnd fieldName, rest)

    parseFieldValue :: Text -> Maybe Text
    parseFieldValue rest =
        case T.head rest of
            ':' -> case T.breakOn "," (T.stripStart (T.tail rest)) of
                (fieldVal, _) -> Just (T.stripEnd fieldVal)
            _ -> Nothing

formatExportStatement :: (Text, Text) -> Text
formatExportStatement (fieldName, fieldValue) =
    T.concat ["exports[\"", fieldName, "\"] = ", fieldValue, ";"]



jsModuleHeader :: Text
jsModuleHeader = "(function(exports) {"

jsModuleFooter :: Namespace -> ModuleName -> Text
jsModuleFooter namespace modulename = T.concat["})(", namespace, "[\"", modulename, "\"] = ", namespace, "[\"", modulename, "\"] || {});"]


createImportSection ::
  Namespace
  -> ModuleName
  -> Vector Text
  -> Maybe (Int, Int)
  -> (Vector Text, [ModuleName], Bool) -- ^ (import lines, req modules, foreign.js module required)
createImportSection _ _ _ Nothing = (V.empty, [], False)
createImportSection namespace modulename ls (Just (firstIndex, numImports)) =
    let sliced = V.slice firstIndex numImports ls
    in V.foldl' next (V.empty, [], False) sliced
    where
    next :: (Vector Text, [ModuleName], Bool) -> Text -> (Vector Text, [ModuleName], Bool)
    next (createdLines, foundReqModules, needsForeign) line =
        case parseImportStatement line of
            Nothing -> (createdLines, foundReqModules, needsForeign)
            Just (varName, reqModule) -> case reqModule == "./foreign.js" of
                True -> let newLine = formatImport namespace (varName, modulename) -- Load the `modulename` instead of trying to load "./foreign"
                    in (createdLines `V.snoc` newLine, foundReqModules, True)
                False -> let newLine = formatImport namespace (varName, reqModule)
                    in (createdLines `V.snoc` newLine, reqModule:foundReqModules, needsForeign)

createExportSection :: Vector Text -> Maybe Int -> Vector Text
createExportSection _ Nothing = V.empty
createExportSection ls (Just exportSectionIndex) =
    let sliced = V.drop exportSectionIndex ls
    in V.foldl' next V.empty sliced
    where
    next :: Vector Text -> Text -> Vector Text
    next createdLines line =
        case parseExportStatement line of
            Nothing -> createdLines
            Just (fieldName, fieldValue) ->
                let newLine = formatExportStatement (fieldName, fieldValue)
                in createdLines `V.snoc` newLine

-- Loads the source code of the foreign.js file, using the supplied loader
-- (usually from disk) and adds the necessary header and footer
processForeign :: Namespace -> ModuleName -> IO Text -> IO (Vector Text)
processForeign namespace modulename loadForeign = do
    let header = jsModuleHeader
    contents <- loadForeign
    let footer = jsModuleFooter namespace modulename
    return $ V.fromList [header, contents, footer]


findImportSection :: Vector Text -> Maybe (Int, Int)
findImportSection ls =
    case V.findIndex (isJust . parseImportStatement) ls of
        Nothing -> Nothing
        Just firstIndex -> case V.findIndex (isNothing . parseImportStatement) (V.drop firstIndex ls) of
            Nothing -> Nothing
            Just numImports -> Just (firstIndex, numImports)

findExportSectionIndex :: Vector Text -> Maybe Int
findExportSectionIndex ls = vectorFindLastIndex isExportLine ls
    where
    isExportLine l =
        let stripped = T.strip l
        in     stripped == "module.exports = {"
            || stripped == "module.exports = {};"

vectorFindLastIndex :: (a -> Bool) -> Vector a -> Maybe Int
vectorFindLastIndex p v = find' ((V.length v) - 1)
    where
    find' (-1) = Nothing
    find' index =
        if p (v V.! index) then Just index
            else find' (index - 1)

