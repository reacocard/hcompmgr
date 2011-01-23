




-- open display
-- get screen
-- get root
-- query extensions?

import Data.Bits
import Data.Maybe
import Control.Monad
import Foreign.C.Types( CInt )
import Foreign.Ptr
import Foreign.Marshal( alloca )
import Foreign.Storable( peek )
import System.IO

import Graphics.X11.Xdamage
import Graphics.X11.Types
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Display
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Event

data Win = Win {  window :: Window
                , damage :: Maybe Damage
                , damaged :: Bool
               } deriving (Show, Eq)

-- Find a Win in winlist by its corresponding X window
findWin :: [Win] -> Window -> Maybe Win
findWin winlist win 
    | null matched = Nothing
    | otherwise = Just $ head matched
    where matched = filter (\x -> window x == win) winlist

-- Replace a Win in winlist with an updated version
updateWin :: [Win] -> Win -> [Win]
updateWin winlist win = [win] ++ (filter (\x -> x /= win) winlist)

handleDamage :: Display -> [Win] -> XEventPtr -> IO [Win]
handleDamage display winlist e = do
    event_win <- get_Window e
    let maybewin = findWin winlist $ event_win
    if isNothing maybewin then return winlist
        else do
            let win = fromJust maybewin
            let maybedamage = damage win
            if isNothing maybedamage then return winlist
                else do
                    xdamageSubtract display (fromJust maybedamage) nullPtr nullPtr
                    return $ updateWin winlist $ win { damaged=True }

eventHandler :: Display -> [Win] -> CInt -> Window -> EventType -> XEventPtr -> IO [Win]
eventHandler display winlist damage_ver event_win evtype e
    | evtype == createNotify = addWin display winlist event_win
    | evtype == configureNotify = defaultHandler
    | evtype == destroyNotify = defaultHandler
    | evtype == mapNotify = defaultHandler
    | evtype == unmapNotify = defaultHandler
    | evtype == reparentNotify = defaultHandler
    | evtype == circulateNotify =  defaultHandler
    | evtype == expose = defaultHandler
    | evtype == propertyNotify = defaultHandler
    | evtype == fromIntegral damage_ver = handleDamage display winlist e
    | otherwise = defaultHandler
    where
        defaultHandler = return winlist

eventLoop :: Display -> [Win] -> CInt -> XEventPtr -> IO [Win]
eventLoop display winlist damage_ver e = do
    nextEvent display e
    evtype <- get_EventType e
    event_win <- get_Window e
    when ((show event_win) /= "176") $ -- root window
        print $ (evtypeToString evtype) ++ " event on window " ++ (show event_win) 
        
    eventHandler display winlist damage_ver event_win evtype e
    where
        evtypeToString evtype
            | evtype == createNotify = "Create"
            | evtype == configureNotify = "Configure"
            | evtype == destroyNotify = "Destroy"
            | evtype == mapNotify = "Map"
            | evtype == unmapNotify = "Unmap"
            | evtype == reparentNotify = "Reparent"
            | evtype == circulateNotify = "Circulate"
            | evtype == expose = "Expose"
            | evtype == propertyNotify = "Property"
            | evtype == fromIntegral damage_ver = "Damage"
            | otherwise = "Unknown (" ++ (show evtype) ++ ")"


addWin :: Display -> [Win] -> Window -> IO [Win]
addWin display winlist win = do
    if isJust $ findWin winlist win then do
        return winlist
        else do
        alloca $ \wa -> do
            status <- xGetWindowAttributes display win wa
            if status == 0 then return winlist
                else do
                attrs <- peek wa
                damage <- if wa_class attrs == inputOutput then do
                    d <- xdamageCreate display win 3 -- XDamageReportNonEmpty = 3 
                    return $ Just d
                    else
                    return Nothing
                name <- fetchName display win
                return $ winlist ++ [Win {window=win, damage=damage, damaged=False}]

main :: IO ()
main = do
    display <- openDisplay ""
    let screen = defaultScreen display
    rootwin <- rootWindow display screen
    damage <- xdamageQueryExtension display
    if isNothing damage then do
        print "XDamage extension not available, exiting."
        return ()
        else do
        let damage_ver = fst $ fromJust damage
            damage_err = snd $ fromJust damage

        print $ "DamageVer: " ++ (show damage_ver)

        selectInput display rootwin  $  substructureNotifyMask
                                    .|. exposureMask
                                    .|. structureNotifyMask 
                                    .|. propertyChangeMask

        sync display False
        xSetErrorHandler

        hSetBuffering stdout NoBuffering

        print $ "ROOT" ++ show rootwin

        grabServer display
        window_query <- queryTree display rootwin
        let children = (\(_,_,x) -> x) window_query
        let winlist = [] :: [Win]
        winlist <- foldM (addWin display) winlist children
        winlist <- addWin display winlist rootwin
        ungrabServer display
        print $ "Winlist: " ++ (show winlist)


        allocaXEvent $ \e -> do
            let mainloop = \wlist evptr -> do
                    winlist <- eventLoop display wlist damage_ver evptr
                    mainloop winlist evptr
            mainloop winlist e
            


