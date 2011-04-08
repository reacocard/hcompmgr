
module GLXRender where

import Foreign.Marshal.Alloc(alloca)
import Foreign(peek, withArray)
import Foreign.C.Types(CULong)
import Foreign.Ptr(nullPtr)
import Data.Maybe
import Data.Bits((.&.), shiftR)

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw as GLR
import Graphics.Rendering.OpenGL (($=))

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.GLX
import Graphics.X11.GLX.Extensions
import Graphics.X11.Xcomposite

import Common

attributes :: [GLXAttribute]
attributes = [ 
--               glxConfigCaveat          , glxNone
--             , glxRenderType            , glxRgbaBit
               glxDoubleBuffer          
             , glxRGBA                  
--             , glxStencilSize, 2
             , glxRedSize               , 8
             , glxGreenSize             , 8
             , glxBlueSize              , 8
             , glxAlphaSize             , 8
--             , glxDepthSize             , 24
             , glxBindToTextureRgbaExt  
             ]

data GLXRenderEngine = GLXRenderEngine { 
          glxr_window :: Window
        , glxr_visual :: XVisualInfo
        }



-- Create checkerboard image to test texturing
checkImageSize :: TextureSize2D
checkImageSize = TextureSize2D 64 64
withCheckImage :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withCheckImage (TextureSize2D w h) n f act =
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | (i .&. n) == (j .&. n) = 0
                     | otherwise              = 255 ] $
   act . PixelData RGBA UnsignedByte

withTestImage (TextureSize2D w h) n f act =
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | j == i = 0
                     | otherwise              = 255 ] $
   act . PixelData RGBA UnsignedByte


checkImage (TextureSize2D w h) n f  =  [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | (i .&. n) == (j .&. n) = 0
                     | otherwise              = 255 ]

 
withX11Image (TextureSize2D width height) image act = do
    pixels <- mapM (\y -> mapM (\x -> getPixel image x y) [0..(fromIntegral $ width-1)]) [0..(fromIntegral $ height-1)]
    colors <- return $ map (\pix -> Color4 
                (fromIntegral (shiftR pix 16) :: GLubyte)
                (fromIntegral ((shiftR pix 8) .&. 255) :: GLubyte)
                (fromIntegral (pix .&. 255) :: GLubyte )
                (255 :: GLubyte)
                ) $ concat pixels
    withArray colors $
        act . PixelData RGBA UnsignedByte
 

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
                    GL.clearColor $= Color4 0 0 0 0
                    GL.texture GL.Texture2D $= GL.Enabled
                    [textureName] <- GL.genObjectNames  1
                    GL.textureBinding GL.Texture2D $= Just textureName
                    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
                    GL.ortho2D (0) 1680 (0) 1050
                    return $ Just $ GLXRenderEngine { glxr_window=compwin, glxr_visual=vis }

                    if ok then return $ Just $ GLXRenderEngine { glxr_window=compwin, glxr_visual=vis }
                          else return Nothing

vertex2f :: (GLfloat, GLfloat) -> GL.Vertex2 GLfloat
vertex2f (x,y) = GL.Vertex2 x y

texCoord2f :: (GLfloat, GLfloat) -> GL.TexCoord2 GLfloat
texCoord2f (u,v) =  GL.TexCoord2 u v
 
color3f :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3f = GL.Color3  

paintAll :: Display -> GLXRenderEngine -> [Win] -> IO [Win]
paintAll dpy render winlist = do
    print "PAINT"
    --tfpPaintAll dpy render winlist
    softwarePaintAll dpy render winlist

tfpPaintAll dpy render winlist = do 
    GL.clear [GL.ColorBuffer]
    _winlist <- mapM (updateWinPixmap dpy) winlist
    _winlist <- mapM (updateWinGLPixmap dpy render) _winlist
    mapM_ (tfpPaintWin dpy render) _winlist
    glXSwapBuffers dpy $ glxr_window render
    --sync dpy False
    return _winlist

updateWinPixmap :: Display -> Win -> IO Win
updateWinPixmap dpy win = 
    if isJust $ (win_glpixmap win) >> (win_pixmap win)
        then return win
        else do
            wa <- getWindowAttributes dpy (win_window win)
            if wa_map_state wa == 0 
                then return win
                else do
                    pixmap <- xcompositeNameWindowPixmap dpy window
                    print "PIXMAP"
                    return $ win { win_pixmap=(Just pixmap) }
    where
        window  = win_window win

updateWinGLPixmap :: Display -> GLXRenderEngine -> Win -> IO Win
updateWinGLPixmap dpy render win =
    if (isNothing $ win_glpixmap win) && (isJust $ win_pixmap win)
        then do
            glPixmap <- glXCreateGLXPixmap dpy xvi $ fromJust $ win_pixmap win
            print "GL PIXMAP"
            return $ win { win_glpixmap=(Just glPixmap) }
        else return win
    where
        xvi     = glxr_visual render        

tfpPaintWin :: Display -> GLXRenderEngine -> Win -> IO ()
tfpPaintWin dpy render win = do
    case win_glpixmap win of
        Nothing -> return ()
        Just glPixmap -> do
            let window = win_window win
            attrs <- getWindowAttributes dpy window
            let width = (wa_width attrs)
                height = (wa_height attrs)
                xpos = wa_x attrs
                ypos = 1050 - (wa_y attrs)

            glXBindTexImageEXT dpy glPixmap glxFrontLeftExt []
            GL.color $ color3f 1 1 1
            GL.renderPrimitive GL.Quads $ do
                GL.vertex $ vertex2f (fromIntegral $ xpos,fromIntegral $ ypos-height)
                GL.texCoord $ texCoord2f (0,0)
                GL.vertex $ vertex2f (fromIntegral $ xpos,fromIntegral $ ypos)
                GL.texCoord $ texCoord2f (1,0)
                GL.vertex $ vertex2f (fromIntegral $ xpos+width,fromIntegral $ ypos)
                GL.texCoord $ texCoord2f (1,1)
                GL.vertex $ vertex2f (fromIntegral $ xpos+width,fromIntegral $ ypos-height)
                GL.texCoord $ texCoord2f (0,1)

            glXReleaseTexImageEXT dpy glPixmap glxFrontLeftExt


softwarePaintAll dpy render winlist = do 
    GL.clear [GL.ColorBuffer]
    mapM_ (softwarePaintWin dpy render) winlist
    glXSwapBuffers dpy $ glxr_window render
    --sync dpy False
    return winlist
    
softwarePaintWin dpy render win = do
    let window = win_window win
    attrs <- getWindowAttributes dpy window
    let width = (wa_width attrs)
        height = (wa_height attrs)
    if (width < 0) || (height < 0) then return () else do
        mImage <- catch (do
            -- FIXME: figure out what the right value for the planes mask is - currently 2^24 - 1
            image <- getImage dpy window 0 0 (fromIntegral width) (fromIntegral height) 16777215 xyPixmap 
            return $ Just image
         ) (\err -> return Nothing)
        case mImage of
            Nothing -> return ()
            Just image -> do
                --writeImage ("windows/" ++ (show window) ++ ".ppm") image width height
                let size = TextureSize2D (fromIntegral width) (fromIntegral height)
                withX11Image size image $ GL.texImage2D Nothing NoProxy 0  RGBA' size 0
                let xpos = wa_x attrs
                    ypos = 1050 - (wa_y attrs)

                GL.color $ color3f 1 1 1
                GL.renderPrimitive GL.Quads $ do
                    GL.vertex $ vertex2f (fromIntegral $ xpos,fromIntegral $ ypos-height)
                    GL.texCoord $ texCoord2f (0,0)
                    GL.vertex $ vertex2f (fromIntegral $ xpos,fromIntegral $ ypos)
                    GL.texCoord $ texCoord2f (1,0)
                    GL.vertex $ vertex2f (fromIntegral $ xpos+width,fromIntegral $ ypos)
                    GL.texCoord $ texCoord2f (1,1)
                    GL.vertex $ vertex2f (fromIntegral $ xpos+width,fromIntegral $ ypos-height)
                    GL.texCoord $ texCoord2f (0,1)

                return ()

writeImage filename image width height = do
    let header = "P3 " ++ (show width) ++ " " ++ (show height) ++ " 255" ++ "\n"
 
    body <- mapM
        (\x -> mapM
            (\y -> do
                pix <- getPixel image y x
                let r = shiftR pix 16
                    g = (shiftR pix 8) .&. 255
                    b = pix .&. 255
                return $ (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ "\n"
            ) 
            [0..(width-1)])
        [0..(height-1)]
    let body2 = concat $ concat body

    writeFile filename $ header ++ body2
    
    return 0
    

