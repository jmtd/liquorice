{-
    Re-imagining of WadC's blockmap.wl, to demonstrate randomness

    We'll abuse things to use as waypoints for where rooms can be
    placed. They record a coord and orientation. We can use a dummy
    thing value, filter on it to find waypoints, and remove them
    all at the end (use healthpotion and leave them in for debugging)
-}

module Main where

import Liquorice
import Liquorice.Pure (step', rand)
import Liquorice.Monad
import Liquorice.Render
import Control.Monad.State.Lazy

import Data.List

basicRoom = do
    markOccupied
    box 256 256 0 160 160
    innerArrow

    -- 3 possibleFutureRooms
    place 256 0 waypoint
    turnleft >> waypoint >> turnright
    place 256 256 (turnright >> waypoint >> turnleft)

    step 256 0

innerArrow = pushpop $ do
        step 64 96
        draw 96 0
        draw 0 (-32)
        draw 64 64
        draw (-64) 64
        draw 0 (-32)
        draw (-96) 0
        draw 0 (-64)
        innerrightsector 0 160 0

rightTurn = do
    markOccupied
    step 0 64
    straight 192
    right 192
    place 0 (-64) waypoint
    right 128
    right 64
    left 64
    right 128
    turnright
    rightsector 0 160 160
    step (192+64) 192
    turnright

leftTurn = do
    markOccupied
    step 0 64
    straight 64
    left 64
    place 0 (-64) waypoint
    right 128
    right 192
    right 192
    right 128
    turnright
    rightsector 0 160 160

teeRoom = do
  markOccupied
  step 0 64
  straight 64
  left 64
  place 0 (-64) waypoint
  right 128
  right 256
  place 0 (-64) waypoint
  right 128
  right 64
  left 64
  right 128
  --turnright
  rightsector 0 160 160

rooms = [basicRoom, leftTurn, rightTurn, teeRoom]

main = buildWad "blockmap.wad" $ runWadL $ do
    setseed 0
    place 64 64 thing
    waypoint'
    basicRoom
    replicateM_ 24 randomRoom

randomRoom = do
    c  <- get
    spot <- chooseSpot
    setLoc (thingPos spot, angleToOrientation (thingAngle spot))
    step (-128) (-128)
    rooms !! (rand c `mod` (length rooms))

chooseSpot :: State Context Thing
chooseSpot = do
    ts <- waypoints
    c  <- get
    if length ts < 1 then error "no waypoints left" else do
        let t = ts !! (rand c `mod` (length ts))
        put $ c { things = filter ((/=)t) (things c) }
        return t

------------------------------------------------------------------------------
--Waypoints
--
-- We (ab)use Things to mark a 256x256 region of the map for a future room.  We
-- use healthpotion and spiritarmor types for debugging; later we could use an
-- invalid tid and post-process the map to remove any left over.

freeMarkerNum     = 2014 -- healthpotion
occupiedMarkerNum = 2015 -- spiritarmor

-- fetch all current open waypoints
waypoints :: State Context [Thing]
waypoints = do
    c <- get
    let ts = filter (\t -> thingType t == freeMarkerNum) (things c)
    return ts

-- mark a waypoint at the current location and orientation
-- *NO* checking to see if it's already occupied
waypoint' = do
    setthing (fromIntegral freeMarkerNum)
    place 128 128 thing

removeWaypoint = do
    c <- get
    ((x,y),facing) <- getLoc
    let loc = step' (x,y) facing 128 128
    let ts = filter (\t -> not (thingType t == freeMarkerNum && thingPos t == loc)) (things c)
    put c { things = ts }

-- We can ignore Orientation because we are marking the middle of the block. If we didn't,
-- North:(0,0) and South:(128,0) would be two different markers for the same block.
-- This does not prevent multiple waypoints on the same coordinate, but that's ok; it
-- increases the chance of this coordinate being chosen, and they are all removed when
-- one is picked.
isOccupied :: Point -> State Context Bool
isOccupied p = do
    c <- get
    let ts = map thingPos
           $ filter (\t -> thingType t == occupiedMarkerNum)
           $ things c
    return $ p `elem` ts

markOccupied = do
    removeWaypoint
    setthing (fromIntegral occupiedMarkerNum)
    place 128 128 thing

waypoint = do
    ((x,y),orient) <- getLoc
    let loc = step' (x,y) orient 128 128
    occupied <- isOccupied loc
    if occupied
    then return ()
    else waypoint'
