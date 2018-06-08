{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Denominator
import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

codeHighlight :: WidgetFor App ()
codeHighlight = do
  addStylesheetRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css"
  addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
  toWidget [julius|
    hljs.initHighlightingOnLoad();
  |]

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getWhyR :: Handler Html
getWhyR = do
  presentationLayout (Just HomeR) (Just WhyTypeSafeR) "Why Yesod?" $ do
    [whamlet|
<ul .bullet-points>
  <li>Template Haskell to remove boilerplate
  <li>Fast execution
  <li>Fast enough development
  <li>Easier to maintain
  <li>Type Safe
|]

getWhyTypeSafeR :: Handler Html
getWhyTypeSafeR = do
  presentationLayout (Just WhyR) (Just TypesR) "Why Type Safe?" $ do
    [whamlet|
<ul .bullet-points>
  <li>What is a type?
  <li>Why do we care about safety?
|]


getTypesR :: Handler Html
getTypesR = do
  presentationLayout (Just WhyTypeSafeR) (Just TypesGraphicR) "Types" $ do
    codeHighlight
    [whamlet|
<ul .bullet-points>
  <li>Word8 =>  0..255
  <li>Text => All possible strings
  <li>Maybe a => Something that may or may not exist
|]

getTypesGraphicR :: Handler Html
getTypesGraphicR = do
  presentationLayout (Just TypesR) (Just TypesDivideR) "Type Illustration" $ do
    codeHighlight
    [whamlet|
<pre>
  <code .haskell>
    data Maybe a = Nothing | Just a
<img src="@{StaticR img_types_png}" alt="types">
|]


getTypesDivideR :: Handler Html
getTypesDivideR = do
  let mResult :: Maybe Int
      mResult = do
        d <- newDenominator 10
        Just $ safeDivide 100 d
  presentationLayout (Just TypesGraphicR) (Just TypesSafetyR) "Now for Some Code!" $ do
    codeHighlight
    [whamlet|
<pre>
  <code .haskell>
    #{denominatorCode}
    $maybe r <- mResult
      #{r}
    $nothing
      uh oh a problem
|]
  where
    denominatorCode :: Text
    denominatorCode =
      "newtype Denominator = Denominator Int\n\
      \\n\
      \newDenominator :: Int -> Maybe Denominator\n\
      \newDenominator i = \n\
      \  if i == 0 then\n\
      \    Nothing\n\
      \  else\n\
      \    Just (Denominator i)\n\
      \\n\
      \safeDivide :: Int -> Denominator -> Int\n\
      \safeDivide numerator (Denominator denominator) = \n\
      \  numerator `div` denominator\n\
      \\n\
      \...\n\
      \\n\
      \case newDenominator 10 of\n\
      \  Nothing ->\n\
      \    error \"uh oh a problem\"\n\
      \  Just d ->\n\
      \    safeDivide 100 d\n\
      \\n\
      \...\n\
      \\n\
      \RESULT:\n"


getTypesSafetyR :: Handler Html
getTypesSafetyR = do
  presentationLayout (Just TypesDivideR) (Just TypesYesodR) "Type Safety" $ do
    codeHighlight
    [whamlet|
<h2 .center-vert>"Making Impossible States Impossible" by Richard Feldman
<p .center-vert>
  <a href="https://www.youtube.com/watch?v=IcgmSRJHu_8">https://www.youtube.com/watch?v=IcgmSRJHu_8
<p .center-vert>
  <img src=@{StaticR img_string_string_string_png} alt="Types are a lie" style="height:450px">
|]


getTypesYesodR :: Handler Html
getTypesYesodR = do
  presentationLayout (Just TypesSafetyR) Nothing "Type Safety with Yesod" $ do
    codeHighlight
    [whamlet|
<p .center-vert>
  <img src=@{StaticR img_yesod_png} alt="Types are a lie" style="height:450px">
|]

getIntroR :: Handler Html
getIntroR = do
  presentationLayout Nothing Nothing "" $ do
    [whamlet|
<ul .bullet-points>
  <li>Body
|]

getMinimalR :: Handler Html
getMinimalR = do
  presentationLayout Nothing Nothing "" $ do
    codeHighlight
    [whamlet|
<pre>
  <code .haskell>
    #{codeText}
<p>
  Provided by https://github.com/parsonsmatt/yesod-minimal/blob/master/src/Minimal.hs
|]
  where
    codeText :: Text
    codeText =
      "{-# LANGUAGE MultiParamTypeClasses #-} \n\
      \{-# LANGUAGE OverloadedStrings     #-} \n\
      \{-# LANGUAGE QuasiQuotes           #-} \n\
      \{-# LANGUAGE TemplateHaskell       #-} \n\
      \{-# LANGUAGE TypeFamilies          #-} \n\
      \ \n\
      \module Minimal where \n\
      \ \n\
      \import           Data.Text                (Text) \n\
      \import           Network.Wai.Handler.Warp (run) \n\
      \import           Yesod.Core               (RenderRoute (..), Yesod, mkYesod, \n\
      \                                           parseRoutes, toWaiApp) \n\
      \ \n\
      \-- | This is my data type. There are many like it, but this one is mine. \n\
      \data Minimal = Minimal \n\
      \ \n\
      \mkYesod \"Minimal\" [parseRoutes| \n\
      \    / RootR GET \n\
      \|] \n\
      \ \n\
      \instance Yesod Minimal \n\
      \ \n\
      \getRootR :: Handler Text \n\
      \getRootR = pure \"Hello, world!\" \n\
      \ \n\
      \main :: IO () \n\
      \main = run 3000 =<< toWaiApp Minimal"


getFoundationR :: Handler Html
getFoundationR = do
  presentationLayout Nothing Nothing "" $ do
    [whamlet|
<ul .bullet-points>
  <li>Body
|]


getTemplatesR :: Handler Html
getTemplatesR = do
  presentationLayout Nothing Nothing "" $ do
    [whamlet|
<ul .bullet-points>
  <li>Body
|]


getToolsR :: Handler Html
getToolsR = do
  presentationLayout Nothing Nothing "" $ do
    [whamlet|
<ul .bullet-points>
  <li>stack templates
  <li>stack new my-test-site yesod-postgres
  <li>stack ghci
  <li>ghcid
|]


getResourcesR :: Handler Html
getResourcesR = do
  presentationLayout Nothing Nothing "" $ do
    [whamlet|
<ul .bullet-points>
  <li>
    <a href="https://www.yesodweb.com/">https://www.yesodweb.com/
|]



-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    -- let submission = Nothing :: Maybe FileForm
    --     handlerName = "getHomeR" :: Text
    presentationLayout Nothing (Just WhyR) "Welcome To Yesod Basics Presentation!" $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        $(widgetFile "homepage")

-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing

--     presentationLayout Nothing Nothing $ do
--         let (commentFormId, commentTextareaId, commentListId) = commentIds
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")

-- sampleForm :: Form FileForm
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField textSettings Nothing
--     -- Add attributes like the placeholder and CSS classes.
--     where textSettings = FieldSettings
--             { fsLabel = "What's on the file?"
--             , fsTooltip = Nothing
--             , fsId = Nothing
--             , fsName = Nothing
--             , fsAttrs =
--                 [ ("class", "form-control")
--                 , ("placeholder", "File description")
--                 ]
--             }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
