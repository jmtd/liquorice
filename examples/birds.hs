{-
    A Liquorice conversion of WadC's "birds.wl" - adjusted to Doom E2M8
    Requires a limit-removing port
 -}
import Control.Monad.State.Lazy
import Data.Tuple (swap)

import Liquorice
import Liquorice.Render
import Liquorice.Monad

-- stuff to move elsewhere ---------------------------------------------------

right x = turnright >> straight x
left x = turnleft >> straight x

cluster stuff dist = do
  turnaround
  step dist dist
  turnaround
  pushpop $ triple $ do
    pushpop $ triple $ do
        stuff
        step dist 0
    step 0 dist

surround stuff dist = do
    pushpop $ do
        step (-1 * dist) (-1 * dist)
        quad $ do
            twice $ do
                stuff
                step dist 0
            stuff
            turnright

getLoc :: State Context (Point, Orientation)
getLoc = do
    ctx <- get
    return (location ctx, orientation ctx)

setLoc :: (Point, Orientation) -> State Context ()
setLoc (p,o) = do
    ctx <- get
    put ctx { location = p, orientation = o }

getLocAt x y = do
    step x y
    l <- getLoc
    step (negate x) (negate y)
    return l

deathmatchstart = setthing 11
boxofrockets = setthing 2046
rocket = setthing 2010
stimpak = setthing 2011
greenarmor = setthing 2018
rocketlauncher = setthing 2003
lostsoul = setthing 3006
chaingun = setthing 2002
healthpotion = setthing 2014
spiritarmor  = setthing 2015
cyberdemon = setthing 16
shortredfirestick = setthing 57
player1start = setthing 1
player2start = setthing 2
player3start = setthing 3
player4start = setthing 4
teleportdest = setthing 14
candle = setthing 34
soulsphere = setthing 2013

floor_w1_down_HnF = 19
floor_w1_down_LnF_TxTy = 37
crusher_w1_slow = 0 -- XXX real value
exit_w1_normal = 52
teleport_wr = 97

extendsector :: State Context ()
extendsector = do
    old <- get
    let lines = linedefs old
        olds  = head (sectors old)
        news  = olds { sectorLines = sectorLines olds ++ lines }
        new   = old { sectors = news : (tail (sectors old)), linedefs = [] }
    put new

-- constants for this map ----------------------------------------------------

skyheight    = 192
wallheight   = 96
turretheight = 160
ceilheight   = 136
skybright    = 150
housebright  = 200

-- nasty way to sort out tag values, temporary
[tomb1,tomb2,tomb3,spoke1,spoke2,spoke3,spoke4,windowtag,pillar2,pillar3,pillar4,crushme,lastbutton,exittag] =
  [1..14]

main = buildWad "birds.wad" $ runWadL $ do
--  undefx
  mapname "E2M8"
  housetex
  start <- getLoc
  ~[north,east,south,west] <- hub

  setLoc north
  spoke starts tomb1 tomb1 floor_w1_down_HnF spiritarmor spoke1
  setLoc east
  spoke (deathmatchstart >> thing >> greenarmor >> thing)
      tomb1 tomb2 floor_w1_down_HnF healthpotion spoke2
  setLoc west
  spoke (chaingun >> thing >> deathmatchstart >> thing)
    tomb2 tomb3 floor_w1_down_HnF healthpotion spoke3
  setLoc south
  spoke (deathmatchstart >> thing >> rocketlauncher >> thing)
    tomb3 windowtag floor_w1_down_LnF_TxTy spiritarmor spoke4

  -- outertag, innertag, walktag, triggertype, outer button tag, outer button type
  setLoc north -- northeast
  courtyard     tomb1 pillar2 pillar3 floor_w1_down_LnF_TxTy 0 0 (soulsphere >> thing >> lostsoul >> surround thing 48)
  setLoc east
  courtyard     tomb2 pillar3 pillar4 floor_w1_down_LnF_TxTy 0 0 (lostsoul >> surround thing 48)
  setLoc south
  courtyard     tomb3 pillar4 lastbutton floor_w1_down_HnF crushme crusher_w1_slow (lostsoul >> surround thing 48)
  setLoc west
  courtyard     windowtag windowtag pillar2 floor_w1_down_LnF_TxTy 0 0 (cyberdemon >> thing)

housetex = do
  floorflat "DEM1_6"
  ceil "DEM1_5"
  upper "MARBLE3"
  mid "MARBLE3"
  lower "MARBLE3"

outdoors = do
  floorflat "MFLR8_4"
  ceil "F_SKY1"
  mid "BROWNHUG"
  lower "BROWNHUG"

hub :: State Context [(Point, Orientation)]
hub = do
  let seg = do
      turnright
      draw 192 0
      draw 128 128
  hub <- getLoc
  turnleft

  west <- getLocAt 0 32
  seg
  north <- getLocAt 0 32
  seg
  east <- getLocAt 0 32
  seg
  south <- getLocAt 0 32
  seg

  rightsector 8 ceilheight housebright
  turnright

  step 0 128
  ceil "F_SKY1"
  floorflat "MFLR8_3"
  edged_ibox (-16) skyheight  (24+housebright)  192  192  32
  step 64 64
  lower "SP_DUDE6"
  floorflat "DEM1_5"
  ibox 64 64 128 ceilheight (48+housebright)
  housetex
  step 32 0
  twice popsector
  return [north,east,south,west]

spoke x tombtag walktag trig ammo spoketag = do
  spoke <- getLoc
  sectortype 0 spoketag
  housetex
  box 512 128 8  ceilheight  housebright
  sectortype 0 0
  ammo
  pushpop $ do
      step 96 64
      triple $ do
          thing -- easyonly
          step 128 0
  pushpop $ do
      step 160 64
      triple $ do
          thing -- easy
          step 128 0
  step 16 32
  sectortype 0 tombtag
  lower "STONGARG"
  withXoff 60 $ ibox 16 64 64 ceilheight housebright
  sectortype 0 0
  popsector
  -- ceiling openings & windows
  setLoc spoke
  step 0 16
  skylights 512
  setLoc spoke
  step 512 (128+16)
  turnaround
  windows 512
  setLoc spoke
  step 512 0
  turnaround
  windows 512

  setLoc spoke
  step 512 0

  -- diamond tip
  let points = [ (64,-64), (64,0), (128,128) ]
  mapM_ (\(x,y) -> draw x y) points
  mapM_ (\(x,y) -> draw x y) (map (\(x,y) -> (-1*x,y)) (reverse points))

  draw  0 (-128)

  rightsector 8 ceilheight housebright

  step 64 0
  nicebutton (-16) ceilheight 200 0 0 trig walktag
  step 16 16
  turnaround
  x

---- nicebutton: draws a nice 64x64 button, with a raised 32x32 button
---- on the inside. Outer and inner lines are set to outer/inner trigger
---- and tag values. The button sector triggers itself (expected to be
---- used with floor lowering types)

nicebutton f c l outertrig outertag innertrig innertag = do
  floorflat "MFLR8_3"
  linetype outertrig outertag
  edged_ibox f c l 104 104 32
  step 20 20
  linetype innertrig innertag
  sectortype 0 innertag
  floorflat "DEM1_6"
  ibox 64 64 (16+f) c l
  sectortype 0 0
  linetype 0 0

--a box (w wide h tall) with a wedge cut out of the corners (wedge length √(2e²))

edged_ibox f c l w h e = do
  step e 0
  straight (h - (2*e))
  draw e e
  turnright
  straight (w - (2*e))
  draw e e
  turnright
  straight (h - (2*e))
  draw e e
  turnright
  straight (w - (2*e))
  draw e e
  turnright
  innerrightsector f c l
  step (-1*e) 0


skylights :: Int -> State Context ()
skylights x = when (x >= 256) $ do
  step 48 0
  edged_ibox 8 (skyheight-32)  (housebright+16) 96 160 16
  ceil "F_SKY1"
  pushpop $ do
      step 17 17
      edged_ibox 8 skyheight (housebright+24) (96-34) (160-34) 8
      housetex
  twice popsector
  step (80+128) 0
  skylights (x-256)

windows :: Int -> State Context ()
windows x = when ((24+128) <= x) $ do
       step 24 0
       sectortype 0 windowtag
       box 128 16 40 128 housebright -- window
       sectortype 0 0
       step 128 0
       windows (x - (24+128))

--a pointy-ceiling. TODO: generalise/extrapolate the 32/16 specifics
--holy crap this is slow :( probably splitlines
step_slope_ceiling f c l =
  mapM_ (\i -> do box 2 16 f (c+i) l >> step 2 0) ([1..32] ++ [32,31..1])

window :: State Context ()
window = do
  loc <- getLoc
  sectortype 0 windowtag
  step_slope_ceiling 40 64 housebright
  setLoc loc
  sectortype 0 0

courtyard outertag innertag walktag triggertype outer_button_tag outer_button_type monsters = do
  step (-56) 232 -- we're now half way along the diagonal hub walls, offset ~8
  loc <- getLoc

  let spokewall = [(88,-88), (416, 0), (64,  0), (64, 64), (64,  0), (144,-144)]

  housetex
  mapM_ (\(x,y) -> draw x y) spokewall

  outdoors
  straight 240
  turnright
  --                       <-------- turret shape -------------->
  let outerwall = [(352,0),(32,32),(0,64),(64,0),(32,32),(32,-32),(320,320)]
  mapM_ (\(x,y) -> draw x y) outerwall
  mapM_ (\(x,y) -> draw x y) (map swap (reverse outerwall))

  turnaround
  straight 240

  housetex
  mapM_ (\(x,y) -> draw x y) $ map (\(x,y) -> (x,-1*y)) (reverse spokewall)

  outdoors
  rightsector 0 256 skybright

  -- three timebombs along the diagonal outside edge of the hub
  rocket
  step (-104) (-72)
  triple $ do
      step 44 44
      thing

  -- new demon cubes
  setLoc loc
  place 64 64 demoncube

  setLoc loc
  outdoors
  step 280 280
  step 32 32
  impbox outertag innertag triggertype walktag outer_button_type outer_button_tag monsters

impbox outertag innertag triggertype walktag outer_button_type outer_button_tag monsters = do
  loc <- getLoc
  ceil "F_SKY1"
  lower "MARBFACE"
  floorflat "DEM1_5"
  sectortype 0 innertag
  ibox 128 128 128 skyheight (skybright+32)
  sectortype 0 0
  pushpop $ do
    step 8 8
    lower "MARBLE3"
    floorflat "MFLR8_3"
    ibox (128-16) (128-16) (-8) skyheight (skybright+32)

    -- exit technique
    pushpop $ do
      step 4 4
      nicebutton 0 skyheight (skybright+32) outer_button_type outer_button_tag triggertype walktag
      step 16 16

  step 64 64
  turnleft
  pushpop monsters
  boxofrockets
  surround thing 40
  setLoc loc
  step (-16) (-16)
  shortredfirestick
  quad $ do
      thing
      step (32+128) 0
      turnright

starts = do
  pushpop $ do
    step (-64) 0
    player1start
    thing
    deathmatchstart
    thing
    step 128 0
    player2start
    thing
    step (-64) (-64)
    player3start
    thing
    step 0 128
    player4start
    thing

-- a 128 cube, with a 45° 128 cube on top
demoncube = do
    floorflat "DEM1_5"
    ceil "DEM1_5"
    lower "MARBFACE"
    upper "MARBFACE"
    step 32 0

    -- the inner octagon
    quad (straight 64 >> draw 32 32 >> turnright) -- Lines 1 & 2
    rightsector 128 128 skybright

    -- outer bigger triangles
    floorflat "MFLR8_4"
    quad $ do
        step 64 0
        withXoff 32 $ draw (-64) 0 -- redrawing Linedef 1
        innerrightsector 0 128 skybright
        draw 32 (-28) -- Linedef 9
        draw 32 28 -- Linedef 10
        extendsector

        step 32 32
        turnright

    -- outer smaller triangles
    floorflat "DEM1_5"
    ceil "F_SKY1"
    quad $ do
        step 96 32
        withXoff 42 $ draw (-32) (-32) -- redrawing Linedef 2
        innerrightsector 128 256 skybright
        draw 32 0 -- Linedef 11
        draw 0 32 -- Linedef 12
        extendsector
        turnright

    twice $ quad $ popsector
    popsector

    -- retrace the outer boundary of the demoncube and tie those lines into
    -- the parent sector
    quad $ do
        withXoff 96   $ straight (-32) -- redrawing linedef 12
        withXoff 0    $ draw 0 32      -- redrawing linedef 11
        withXoff 89   $ draw (-28) 32  -- redrawing linedef 10
        withXoff (-2) $ draw 28 32     -- redrawing linedef 9
        turnleft
    extendsector
