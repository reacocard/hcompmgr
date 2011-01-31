
module GLXRender where

import Data.Maybe(isJust, fromJust)
import Control.Monad(filterM)

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL.Framebuffer

import Graphics.X11.Xlib(Display, ScreenNumber, Window)
import Graphics.X11.GLX
--import Graphics.X11.GLX.Extensions
import Graphics.X11.Xcomposite

import Common

attributes = [ glxDoubleBuffer          , 1
             , glxRGBA                  , 1
             , glxRedSize               , 1
             , glxGreenSize             , 1
             , glxBlueSize              , 1
             , glxAlphaSize             , 1
             , glxDepthSize             , 0
--             , glxBindToTextureRgbaExt  , 1
             ]

chooseFBConfig :: Display -> ScreenNumber -> IO (Maybe GLXFBConfig)
chooseFBConfig dpy screen = do
    mVis <- glXChooseVisual dpy screen attributes
    case mVis of
        Nothing  -> return Nothing
        Just vis -> do
            --fbcs <- glXChooseFBConfig dpy screen attributes
            fbcs <- glXGetFBConfigs dpy screen
            print $ length fbcs
            matches <- filterM (matchVis vis) fbcs
            print $ length matches
            ids <- mapM (\f -> glXGetFBConfigAttrib dpy f glxVisualId) fbcs
            print ids
            return Nothing
    where
        matchVis vis fbc = do
            mFbVisId <- glXGetFBConfigAttrib dpy fbc glxVisualId
            case mFbVisId of
                Nothing       -> return False
                Just fbvisid  -> do id <- xviVisualId vis; return $ id == (fromIntegral fbvisid)

createWindow :: Display -> ScreenNumber -> Window -> IO (Maybe Window)
createWindow dpy screen win = do
    mVis <- glXChooseVisual dpy screen attributes
    chooseFBConfig dpy screen
    case mVis of
        Nothing  -> return Nothing
        Just vis -> do
            mCtx <- glXCreateContext dpy vis Nothing True
            case mCtx of
                Nothing  -> do print "no CTX"; return Nothing
                Just ctx -> do
                    print "GOT CTX"
                    ok <- glXMakeCurrent dpy win ctx
                    if ok then return $ Just win
                          else return Nothing


paintAll :: Display -> Window -> [Win] -> IO ()
paintAll dpy compwin winlist = do
    mapM_ (paintWin dpy compwin) winlist
    GL.flush

paintWin :: Display -> Window -> Win -> IO ()
paintWin dpy compwin win = do
    -- get pixmap (will be elsewhere eventually)
    --pixmap <- xcompositeNameWindowPixmap dpy window
    -- bind to texture
    --glPixmap <- glXCreatePixmap
    -- render texture
    return () 

    where
        window = win_window win
