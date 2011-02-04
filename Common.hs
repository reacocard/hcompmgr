module Common where

import Graphics.X11.Xlib(Window, Pixmap)
import Graphics.X11.Xdamage(Damage)

import Graphics.X11.GLX(GLXPixmap)

data Win =  Win { win_window    :: Window
                , win_damage    :: Maybe Damage
                , win_damaged   :: Bool
                , win_pixmap    :: Maybe Pixmap
                , win_glpixmap  :: Maybe GLXPixmap
                } deriving (Show, Eq)

defaultsWin :: Win
defaultsWin =   Win { win_window    = -1        -- invalid, must be overridden
                    , win_damage    = Nothing
                    , win_damaged   = False
                    , win_pixmap    = Nothing
                    , win_glpixmap  = Nothing
                    }

