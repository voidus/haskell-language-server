{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
module Development.IDE.Core.Language where

import           Control.Concurrent.Extra       (readVar)
import           Control.Concurrent.Strict      (Var, modifyVar_)
import           Control.Lens                   ((^.))
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HashMap
import           Data.Text                      (Text)
import           Development.IDE.Core.Shake     (IdeState, IsIdeGlobal,
                                                 getIdeGlobalState)
import           Development.IDE.Types.Location (NormalizedFilePath, Uri)
import           Ide.Types                      (SupportedLanguage (..))
import           Language.LSP.Types             (SMethod (..),
                                                 toNormalizedFilePath)
import qualified Language.LSP.Types             as LSP
import qualified Language.LSP.Types.Lens        as J

newtype LanguageVar = LanguageVar (Var (HashMap NormalizedFilePath SupportedLanguage))

instance IsIdeGlobal LanguageVar

setFileLanguage :: IdeState -> NormalizedFilePath -> SupportedLanguage -> IO ()
setFileLanguage state path language = do
    LanguageVar var <- getIdeGlobalState state
    modifyVar_ var $ \dict ->
        pure $ HashMap.insert path language dict


getFileLanguage :: IdeState -> NormalizedFilePath -> IO (Maybe SupportedLanguage)
getFileLanguage state path = do
    LanguageVar var <- getIdeGlobalState state
    dict <- readVar var
    pure $ HashMap.lookup path dict

getUriLanguage :: IdeState -> Uri -> IO (Maybe SupportedLanguage)
getUriLanguage state uri =
    case LSP.uriToFilePath uri of
        Just path -> getFileLanguage state $ toNormalizedFilePath path
        Nothing   -> pure Nothing

getParamLanguage :: forall msg. IdeState -> SMethod msg -> LSP.MessageParams msg -> IO (Maybe SupportedLanguage)
getParamLanguage state m params =
    case maybeUri of
        Just uri -> getUriLanguage state uri
        Nothing  -> pure Nothing
    where
        maybeUri :: Maybe Uri
        maybeUri = case m of
            SInitialize -> Nothing
            SInitialized -> Nothing
            SShutdown -> Nothing
            SExit -> Nothing
            SWorkspaceDidChangeWorkspaceFolders -> Nothing
            SWorkspaceDidChangeConfiguration -> Nothing
            SWorkspaceDidChangeWatchedFiles -> Nothing
            SWorkspaceSymbol -> Nothing
            SWorkspaceExecuteCommand -> Nothing
            STextDocumentDidOpen -> Just $ params ^. J.textDocument . J.uri
            STextDocumentDidChange -> Just $ params ^. J.textDocument . J.uri
            STextDocumentWillSave -> Just $ params ^. J.textDocument . J.uri
            STextDocumentWillSaveWaitUntil -> Just $ params ^. J.textDocument . J.uri
            STextDocumentDidSave -> Just $ params ^. J.textDocument . J.uri
            STextDocumentDidClose -> Just $ params ^. J.textDocument . J.uri
            STextDocumentCompletion -> Just $ params ^. J.textDocument . J.uri
            SCompletionItemResolve -> Nothing
            STextDocumentHover -> Just $ params ^. J.textDocument . J.uri
            STextDocumentSignatureHelp -> Just $ params ^. J.textDocument . J.uri
            STextDocumentDeclaration -> Just $ params ^. J.textDocument . J.uri
            STextDocumentDefinition -> Just $ params ^. J.textDocument . J.uri
            STextDocumentTypeDefinition -> Just $ params ^. J.textDocument . J.uri
            STextDocumentImplementation -> Just $ params ^. J.textDocument . J.uri
            STextDocumentReferences -> Just $ params ^. J.textDocument . J.uri
            STextDocumentDocumentHighlight -> Just $ params ^. J.textDocument . J.uri
            STextDocumentDocumentSymbol -> Just $ params ^. J.textDocument . J.uri
            STextDocumentCodeAction -> Just $ params ^. J.textDocument . J.uri
            STextDocumentCodeLens -> Just $ params ^. J.textDocument . J.uri
            SCodeLensResolve -> Nothing
            STextDocumentDocumentLink -> Just $ params ^. J.textDocument . J.uri
            SDocumentLinkResolve -> Nothing
            STextDocumentDocumentColor -> Just $ params ^. J.textDocument . J.uri
            STextDocumentColorPresentation -> Just $ params ^. J.textDocument . J.uri
            STextDocumentFormatting -> Just $ params ^. J.textDocument . J.uri
            STextDocumentRangeFormatting -> Just $ params ^. J.textDocument . J.uri
            STextDocumentOnTypeFormatting -> Just $ params ^. J.textDocument . J.uri
            STextDocumentRename -> Just $ params ^. J.textDocument . J.uri
            STextDocumentPrepareRename -> Just $ params ^. J.textDocument . J.uri
            STextDocumentFoldingRange -> Just $ params ^. J.textDocument . J.uri
            STextDocumentSelectionRange -> Just $ params ^. J.textDocument . J.uri
            STextDocumentPrepareCallHierarchy -> Just $ params ^. J.textDocument . J.uri
            SCallHierarchyIncomingCalls -> Nothing
            SCallHierarchyOutgoingCalls -> Nothing
            STextDocumentSemanticTokens -> Nothing
            STextDocumentSemanticTokensFull -> Just $ params ^. J.textDocument . J.uri
            STextDocumentSemanticTokensFullDelta -> Just $ params ^. J.textDocument . J.uri
            STextDocumentSemanticTokensRange -> Just $ params ^. J.textDocument . J.uri
            SWorkspaceSemanticTokensRefresh -> Nothing
            SWindowShowMessage -> Nothing
            SWindowShowMessageRequest -> Nothing
            SWindowShowDocument -> Nothing
            SWindowLogMessage -> Nothing
            SWindowWorkDoneProgressCreate -> Nothing
            SWindowWorkDoneProgressCancel -> Nothing
            SProgress -> Nothing
            STelemetryEvent -> Nothing
            SClientRegisterCapability -> Nothing
            SClientUnregisterCapability -> Nothing
            SWorkspaceWorkspaceFolders -> Nothing
            SWorkspaceConfiguration -> Nothing
            SWorkspaceApplyEdit -> Nothing
            STextDocumentPublishDiagnostics -> Nothing
            SCancelRequest -> Nothing
            SCustomMethod _ -> Nothing
