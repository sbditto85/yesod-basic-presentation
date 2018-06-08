{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Control.Monad.Logger        (LogSource)
import Text.Hamlet                 (hamletFile)
import Text.Jasmine                (minifym)
import Yesod.Core.Types            (Logger)
import Yesod.Default.Util          (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
-- mkYesodData "App" $(parseRoutesFile "config/routes")
mkYesodData "App" [parseRoutes|
-- By default this file is used by `parseRoutesFile` in Foundation.hs
-- Syntax for this file here: https://www.yesodweb.com/book/routing-and-handlers

/static StaticR Static appStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ HomeR GET
/why WhyR GET
/why/typesafe WhyTypeSafeR GET
/why/typesafe/types TypesR GET
/why/typesafe/types_graphic TypesGraphicR GET
/why/typesafe/types_divide TypesDivideR GET
/why/typesafe/types_safety TypesSafetyR GET
/why/typesafe/types_yesod TypesYesodR GET

/intro IntroR GET

/minimal MinimalR GET

/foundation FoundationR GET

/templates TemplatesR GET

/core CoreR GET

/yesod-auth YesodAuthIllustratedR GET

/tools ToolsR GET

/resources ResourcesR GET

/comments CommentR POST
|]

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Why?"
                    , menuItemRoute = WhyR
                    , menuItemAccessCallback = True
                    }
                -- , NavbarLeft $ MenuItem
                --     { menuItemLabel = "Intro"
                --     , menuItemRoute = IntroR
                --     , menuItemAccessCallback = True
                --     }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Minimal"
                    , menuItemRoute = MinimalR
                    , menuItemAccessCallback = True
                    }
                -- , NavbarLeft $ MenuItem
                --     { menuItemLabel = "Foundation"
                --     , menuItemRoute = FoundationR
                --     , menuItemAccessCallback = True
                --     }
                -- , NavbarLeft $ MenuItem
                --     { menuItemLabel = "Templates"
                --     , menuItemRoute = TemplatesR
                --     , menuItemAccessCallback = True
                --     }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Core Concepts"
                    , menuItemRoute = CoreR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Tools"
                    , menuItemRoute = ToolsR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Resources"
                    , menuItemRoute = ResourcesR
                    , menuItemAccessCallback = True
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    isAuthorized :: Route App  -- ^ The route the user is visiting.
                 -> Bool       -- ^ Whether or not this is a "write" request.
                 -> Handler AuthResult
    -- Routes not requiring authenitcation.
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

presentationLayout :: Maybe (Route App) -- ^ Back route
                   -> Maybe (Route App) -- ^ Forward route
                   -> Text              -- ^ Page title
                   -> Widget            -- ^ Main content
                   -> Handler Html
presentationLayout mBackRoute mForwardRoute title widget =
  defaultLayout $ do
    case mBackRoute of
      Nothing ->
        pure ()
      Just route ->
        backWidget route
    case mForwardRoute of
      Nothing ->
        pure ()
      Just route ->
        forwardWidget route
    mCurrentRoute <- getCurrentRoute
    setTitle $ toHtml title
    [whamlet|
      $case mCurrentRoute
        $of Just HomeR
        $of _
          <h1 .center-vert>#{title}
      ^{widget}
    |]

  where
    backWidget route = do
      toWidget [lucius|
              .back-button {
                position: fixed;
                top: 100px;
                left: 30px;
                button {
                  display: inline-block;
                  height: 600px;
                  width: 100px;
                  background: transparent;
                  border: 0px;
                }
              }
      |]
      [whamlet|
              <div .back-button>
                <button type="button" onclick="location.href='@{route}'"><<
      |]

    forwardWidget route = do
      toWidget [lucius|
              .forward-button {
                position: fixed;
                top: 100px;
                right: 30px;
                button {
                  display: inline-block;
                  height: 600px;
                  width: 100px;
                  background: transparent;
                  border: 0px;
                }
              }
      |]
      [whamlet|
              <div .forward-button>
                <button type="button" onclick="location.href='@{route}'">>>
      |]


-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb :: Route App  -- ^ The route the user is visiting currently.
               -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR                 = pure ("Home", Nothing)
    breadcrumb WhyR                  = pure ("Why?", Just HomeR)
    breadcrumb WhyTypeSafeR          = pure ("Why Type Safe?", Just WhyR)
    breadcrumb TypesR                = pure ("Types", Just WhyTypeSafeR)
    breadcrumb TypesGraphicR         = pure ("Graphic", Just TypesR)
    breadcrumb TypesDivideR          = pure ("Division", Just TypesR)
    breadcrumb TypesSafetyR          = pure ("Safety", Just TypesR)
    breadcrumb TypesYesodR           = pure ("Yesod", Just TypesR)

    breadcrumb IntroR                = pure ("Intro", Just HomeR)

    breadcrumb MinimalR              = pure ("Minimal", Just HomeR)

    breadcrumb FoundationR           = pure ("Foundation", Just HomeR)

    breadcrumb TemplatesR            = pure ("Templates", Just HomeR)

    breadcrumb CoreR                 = pure ("Core", Just HomeR)

    breadcrumb YesodAuthIllustratedR = pure ("Yesod Auth", Just HomeR)

    breadcrumb ToolsR                = pure ("Tools", Just HomeR)

    breadcrumb ResourcesR            = pure ("Resources", Just HomeR)

    breadcrumb (StaticR _)           = pure ("home", Nothing)
    breadcrumb FaviconR              = pure ("home", Nothing)
    breadcrumb RobotsR               = pure ("home", Nothing)
    breadcrumb CommentR              = pure ("home", Nothing)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
