-- simple example, triangle (for orientation); unique texture per line
import Liquorice.Monad
import Render

main = buildWad "example1.wad" $ runWadL $ do
    mid "ZZWOLF1"
    draw 128 0
    mid "ZZWOLF2"
    draw 0 128
    turnaround
    mid "ZZWOLF3"
    draw 128 128
    rightsector 0 128 160
    turnaround
    step 64 32
    thing
