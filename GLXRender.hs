
module GLXRender where

import Data.Bits
import Data.Word
import Foreign.Marshal.Alloc(alloca)
import Foreign.Ptr(nullPtr)
import Foreign(peek)
import Data.Maybe(isJust, fromJust)
import Control.Monad(filterM)

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLR
import Graphics.Rendering.OpenGL.GL.Framebuffer

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.GLX
import Graphics.X11.GLX.Extensions
import Graphics.X11.Xcomposite

import Common

attributes = [ 
--               glxConfigCaveat          , glxNone
--             , glxRenderType            , glxRgbaBit
               glxDoubleBuffer          
             , glxRGBA                  
             , glxRedSize               , 1
--             , glxGreenSize             , 1
--             , glxBlueSize              , 1
--             , glxAlphaSize             , 1
--             , glxDepthSize             , 24
             , glxBindToTextureRgbaExt
             ]

data GLXRenderEngine = GLXRenderEngine { 
          glxr_window :: Window
        , glxr_visual :: XVisualInfo
        }

initRender :: Display -> ScreenNumber -> Window -> IO (Maybe GLXRenderEngine)
initRender dpy screen compwin = do
    mVis <- glXChooseVisual dpy screen attributes
    case mVis of
        Nothing  -> return Nothing
        Just vis -> do
            mCtx <- glXCreateContext dpy vis Nothing False
            case mCtx of
                Nothing  -> return Nothing
                Just ctx -> do
                    ok <- glXMakeCurrent dpy compwin ctx
                    if ok then return $ Just $ GLXRenderEngine { glxr_window=compwin, glxr_visual=vis }
                          else return Nothing


paintAll :: Display -> GLXRenderEngine -> [Win] -> IO [Win]
paintAll dpy render winlist = do
    winlist <- mapM (paintWin dpy render) winlist
    -- paintWin dpy render (winlist !! 0)
    GL.flush
    glXSwapBuffers dpy $ glxr_window render
    sync dpy False
    return winlist

paintWin :: Display -> GLXRenderEngine -> Win -> IO Win
paintWin dpy render win = do
    win <- case win_glpixmap win of
        Just _ -> return win
        Nothing -> do 
            win <- case win_pixmap win of
                Just _  -> return win
                Nothing -> do
                    wa <- getWindowAttributes dpy (win_window win)
                    if wa_map_state wa == 0 
                        then return win
                        else do
                            pixmap <- xcompositeNameWindowPixmap dpy window
                            return $ win { win_pixmap=(Just pixmap) }
            win <- case win_pixmap win of
                Nothing -> return win
                Just pixmap -> do
                    glPixmap <- glXCreateGLXPixmap dpy xvi pixmap
                    return $ win { win_glpixmap=(Just glPixmap) }
            return win
    case win_glpixmap win of
        Nothing -> return win
        Just glPixmap -> do
            GLR.glEnable GLR.gl_TEXTURE_2D

            textureid <- alloca $ \n -> do
                GLR.glGenTextures 1 n
                peek n
            GLR.glBindTexture GLR.gl_TEXTURE_2D textureid

            -- bind to texture
            glXBindTexImageEXT dpy glPixmap glxFrontLeftExt []

            GLR.glTexParameteri GLR.gl_TEXTURE_2D GLR.gl_TEXTURE_MIN_FILTER 0x2601
            GLR.glTexParameteri GLR.gl_TEXTURE_2D GLR.gl_TEXTURE_MAG_FILTER 0x2601

            GLR.glBegin GLR.gl_QUADS

            GLR.glTexCoord2d 0 0
            GLR.glVertex2d 0 0 

            GLR.glTexCoord2d 0 1
            GLR.glVertex2d 0 1

            GLR.glTexCoord2d 1 1
            GLR.glVertex2d 1 1

            GLR.glTexCoord2d 1 0 
            GLR.glVertex2d 1 0

            GLR.glEnd


            glXReleaseTexImageEXT dpy glPixmap glxFrontLeftExt
            sync dpy False

            -- render texture

            return win

    where
        window  = win_window win
        compwin = glxr_window render
        xvi     = glxr_visual render
