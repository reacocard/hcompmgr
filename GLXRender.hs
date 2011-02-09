
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
    rootwin     <- rootWindow dpy screen
    mVis <- glXChooseVisual dpy screen attributes
    case mVis of
        Nothing  -> return Nothing
        Just vis -> do
            visid <- xviVisualId vis
            print $ "Got visual " ++ (show visid)
            mCtx <- glXCreateContext dpy vis Nothing False
            case mCtx of
                Nothing  -> do print "no CTX"; return Nothing
                Just ctx -> do
                    print "GOT CTX"
--                  w <- allocaSetWindowAttributes $ \xwa -> do 
--                      visid <- xviVisualId vis
--                      set_background_pixel xwa (blackPixel dpy (defaultScreen dpy))
--                      set_border_pixel xwa (blackPixel dpy (defaultScreen dpy))
--                      colorMap <- createColormap dpy rootwin visid allocNone
--                      set_colormap xwa colorMap
--                      createWindow dpy rootwin 0 0 1024 768 24 inputOutput visid (cWBackPixel .|. cWBorderPixel .|. cWColormap) xwa
                    ok <- glXMakeCurrent dpy compwin ctx
                    if ok then return $ Just $ GLXRenderEngine { glxr_window=compwin, glxr_visual=vis }
                          else return Nothing

-- Junk, jsut used so we have some feedback that gl is working
glDrawScene =
    do GL.clearColor GL.$= GL.Color4 0 0 0 1
       bMap <- GL.newMap1 (0.0,1.0) [ GL.Vertex3 (-0.9) 0.0 0.0
                                    , GL.Vertex3 (-0.5) 3.0 0.0
                                    , GL.Vertex3 0.5 (-3.0) 0.0
                                    , GL.Vertex3 0.9 0.0 0.0
                                    ] :: IO (GL.GLmap1 GL.Vertex3 GL.GLfloat)
       GL.map1 GL.$= Just bMap
       GL.loadIdentity
       GL.clear [ColorBuffer,DepthBuffer]
       GL.color (GL.Color3 1.0 1.0 1.0 :: GL.Color3 GL.GLfloat)
--       GL.renderPrimitive GL.Points $ mapM_ (\i -> GL.vertex $ GL.Vertex2 i i) ([0.0,0.1.. 1.0] :: [GL.GLfloat])
       GL.renderPrimitive GL.Points $ mapM_ GL.evalCoord1 ([0.0,0.01 .. 1.0] :: [GL.GLfloat])
       GL.flush


paintAll :: Display -> GLXRenderEngine -> [Win] -> IO [Win]
paintAll dpy render winlist = do
--    glDrawScene
    winlist <- mapM (paintWin dpy render) winlist
    -- paintWin dpy render (winlist !! 0)
    GL.flush
    glXSwapBuffers dpy $ glxr_window render
    sync dpy False
    return winlist

paintWin :: Display -> GLXRenderEngine -> Win -> IO Win
paintWin dpy render win = do
    -- get pixmap (will be elsewhere eventually)
    --print $ "Painting window " ++ (show window)
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
                            print $ "Pixmap for " ++ (show window)
                            return $ win { win_pixmap=(Just pixmap) }
            win <- case win_pixmap win of
                Nothing -> return win
                Just pixmap -> do
                    glPixmap <- glXCreateGLXPixmap dpy xvi pixmap
                    print "GOT GL PIXMAP"
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
