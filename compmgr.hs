----------------------------------------------------------------------------
-- |
-- Module      :  compmgr
-- Copyright   :  (c) Aren Olson 2011
-- License     :  BSD3 (see LICENSE)
--
-- Maintainer  :  Aren Olson <reacocard@gmail.com>
-- Stability   :  unstable
-- Portability :  not portable, uses X11
--
-----------------------------------------------------------------------------


import Data.Bits((.|.))
import Data.Maybe
import Data.List (find, delete)
import Control.Monad
import Foreign.Marshal(alloca)
import Foreign.Storable(peek)
import Foreign.C.Types
import System.IO

import Graphics.X11.Types
import Graphics.X11.Xlib hiding (Region) -- we use xfixes' Region
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xdamage
import Graphics.X11.Xfixes
import Graphics.X11.Xcomposite
import Graphics.X11.GLX

import Common
import qualified GLXRender as R




-- translate an EventType to a human-readable name
evtypeToString :: EventType -> EventType -> String
evtypeToString evtype damageNotify
    | evtype == createNotify            = "Create"
    | evtype == configureNotify         = "Configure"
    | evtype == destroyNotify           = "Destroy"
    | evtype == mapNotify               = "Map"
    | evtype == unmapNotify             = "Unmap"
    | evtype == reparentNotify          = "Reparent"
    | evtype == circulateNotify         = "Circulate"
    | evtype == expose                  = "Expose"
    | evtype == propertyNotify          = "Property"
    | evtype == damageNotify            = "Damage"
    | otherwise                         = "Unknown (" ++ (show evtype) ++ ")"

-- Find a Win in winlist by its corresponding X window
findWin :: [Win] -> Window -> Maybe Win
findWin winlist win = find (\x -> win_window x == win) winlist

-- Replace a Win in winlist with an updated version
updateWin :: [Win] -> Win -> [Win]
updateWin winlist win = [win] ++ 
    filter (\x -> win_window x /= win_window win) winlist

-- Remove a Win from the winlist
delWin :: [Win] -> Win -> [Win]
delWin winlist win = delete win winlist

addWin :: [Win] -> Win -> [Win]
addWin winlist win = winlist ++ [win]

winFromWindow :: Display -> Window -> IO (Maybe Win)
winFromWindow display win = alloca $ \wa -> do
    status <- xGetWindowAttributes display win wa
    case status of
        0 -> return Nothing
        _ -> do
            attrs  <- peek wa
            damage <- if wa_class attrs == inputOnly
                        then return Nothing
                        else do d <- xdamageCreate display win 3
                                return $ Just d
            return $ Just $ defaultsWin {win_window=win, win_damage=damage}

eventHandler :: Display -> Window -> [Win] -> EventType -> Event -> IO [Win]
eventHandler display compwin winlist damageNotify ev
    | event_win == compwin = do
            return winlist

    | evtype == createNotify && isNothing maybewin = do
            window <- winFromWindow display event_win
            case window of
                Nothing -> return winlist
                Just w  -> return $ addWin winlist w

    | evtype == destroyNotify && isJust maybewin = do
            when (isJust maybedamage) $ xdamageDestroy display damage
            return $ delWin winlist win

    | evtype == unmapNotify && isJust maybewin = do
            when (isJust $ win_pixmap win) $ freePixmap display $ fromJust $ win_pixmap win
            return $ updateWin winlist $ win { win_pixmap=Nothing }

    | evtype == damageNotify && isJust maybewin && isJust maybedamage = do
            return $ updateWin winlist $ win { win_damaged=True }

    | otherwise = do
            return winlist

    where
        evtype          = ev_event_type ev
        event_win       = ev_window ev
        maybewin        = findWin winlist event_win
        win             = fromJust maybewin
        maybedamage     = win_damage win
        damage          = fromJust maybedamage

printEventDebug :: Event -> EventType -> IO ()
printEventDebug event damageNotify
    -- Ignore damage events on the root window and the window running 
    -- the program to avoid infinite loops
    | (evwin /= 176 && evwin /= 48234502) || evtype /= damageNotify =
        print $ (evtypeToString evtype damageNotify) ++ " event on window " ++ (show evwin)
    | otherwise =
        return ()
    where
        evtype  = ev_event_type event
        evwin   = ev_window     event

removeDamage :: Display -> [Win] -> IO [Win]
removeDamage display winlist = mapM remDamage winlist
    where
        remDamage win = if win_damaged win && isJust (win_damage win) 
            then do
                xdamageSubtract display (fromJust $ win_damage win) none none
                return $ win { win_damaged=False }
            else return win

eventLoop :: Display -> Window -> R.GLXRenderEngine -> [Win] -> EventType -> XEventPtr -> IO [Win]
eventLoop display compwin render winlist damageNotify e = do
    nextEvent display e
    ev <- getEvent e
    printEventDebug ev damageNotify
    winlist <- eventHandler display compwin winlist damageNotify ev
    nPend <- pending display
    --R.paintAll display render winlist
    winlist <- if nPend /= 0
                then return winlist
                else do
                    sync display False
                    grabServer display
                    sync display False
                    winlist <- removeDamage display winlist
                    winlist <- R.paintAll display render winlist
                    ungrabServer display
                    return winlist
    eventLoop display compwin render winlist damageNotify e

main :: IO ()
main = do
    display     <- openDisplay ""
    screen      <- return $ defaultScreen display
    rootwin     <- rootWindow display screen
    damage      <- xdamageQueryExtension display
    composite   <- xcompositeQueryExtension display
    fixes       <- xfixesQueryExtension display
    -- TODO: check extension versions
    case (damage, composite, fixes) of
        (Nothing , _        , _       ) -> do error "Damage extension missing."
        (_       , Nothing  , _       ) -> do error "Composite extension missing."
        (_       , _        , Nothing ) -> do error "Fixes extension missing."
        (Just (damNotify,_), Just _, Just _) -> do
            let damageNotify = fromIntegral damNotify :: EventType

            selectInput display rootwin  $  substructureNotifyMask
                                        .|. exposureMask
                                        .|. structureNotifyMask 
                                        .|. propertyChangeMask
            sync display False
            xSetErrorHandler
            hSetBuffering stdout NoBuffering

            grabServer display
            winlist <- do
                (_,_,winlist) <- queryTree display rootwin
                maybewins     <- mapM (winFromWindow display) winlist
                return $ catMaybes maybewins
            xcompositeRedirectSubwindows display rootwin compositeRedirectManual
            ungrabServer display

            compwin <- xcompositeGetOverlayWindow display rootwin
            mapWindow display compwin

            print $ "DamageVer: "   ++ show damageNotify
            print $ "RootWin: "     ++ show rootwin
            --print $ "CompWin: "     ++ show compwin
            print $ "Winlist: "     ++ show winlist
            
        
            mRender <- R.initRender display screen compwin
            case mRender of
                Nothing -> do error "Could not set up rendering backend."
                Just render -> do
                    allocaXEvent $ \e -> do 
                        eventLoop display compwin render winlist damageNotify e
                        return ()
