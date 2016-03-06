from glob import glob
from math import floor
from os import listdir
from os.path import basename, join
import re

from convutil import Polygon, Point, Texture

LATRES=360.0/32768
LONRES=480.0/32768

texre=re.compile(r"\.(bmp|dds)$", re.IGNORECASE)
photore=re.compile(r"[0-3]{15,15}su\.(bmp|dds)$", re.IGNORECASE)
blueskyre=re.compile(r"S(\d)\$([+-]*\d+)-([+-]*\d+)_(\d)_(\d)\.(bmp|BMP|dds|DDS)$")

# Handle FS2004 and www.blueskyscenery.com photoscenery

# Must be called after processing objects to avoid applying texture paging
# to textures used in objects

def ProcPhoto(texdir, output):

    output.log.debug("%s\n" % texdir)

    # All textures
    texs=[i for i in listdir(texdir) if texre.search(i)]
    texsdict=dict([[i[:-4].lower(),i] for i in texs])

    # Blue Sky Scenery style
    bluesky=[i for i in texs if blueskyre.match(i)]
    blueskydict=dict([[i[:-4],i] for i in bluesky])
    for tex in bluesky:
        match=blueskyre.split(tex)
        if len(match)<6: continue
        lat=int(match[2])
        lon=int(match[3])
        layer=int(match[1])
        res=0.125*layer
        name=tex[:-4]

        if ishigher(name, layer, lat,lon, blueskydict, output): continue

        # findtex is too slow
        if name+'_lm' in texsdict:
            lit=join(texdir,texsdict[name+'_lm'])
        else:
            lit=None

        lat=(lat-int(match[4])*res)*LATRES
        lon=(lon+int(match[5])*res)*LONRES
        layer=max(1,4-layer)
        makephoto(name, Texture(output.xpver, join(texdir,tex), lit), lat, lon, res, layer, 512, output)

    # Standard FS2004 style - assumes that summer exists
    for tex in texs:
        if not photore.match(tex): continue
        name=tex[:-6]
        seasonal=name+["sp","su","fa","wi"][output.season]
        if seasonal in texsdict: tex=texsdict[seasonal]
        # find NW point
        lat=lon=0
        for i in tex[-21:-6]:
            lat += lat
            lon += lon
            if i=='1':
                lon += 1
            elif i=='2':
                lat += 1
            elif i=='3':
                lat += 1
                lon += 1
        lat=8192-lat	# = 90
        lon -= 12288  # = -180

        if ishigher(name, 0, lat,lon, blueskydict, output): continue

        # findtex is too slow
        if name+'lm' in texsdict:
            lit=join(texdir,texsdict[name+'lm'])
        else:
            lit=None

        lat *= LATRES
        lon *= LONRES
        makephoto(name, Texture(output.xpver, join(texdir,tex), lit), lat, lon, 1, 0, 256, output)


# Check for higher-res Blue Sky Scenery scenery
def ishigher(name, layer, lat,lon, blueskydict, output):
    bstr="%d-%d" % (lat,lon)
    for (res,n) in [(1,8),(2,4),(4,2)]:
        if res==layer: return False
        hires=True
        resstr="S%d$%s" % (res,bstr)
        for i in range(n):
            for j in range(n):
                if ("%s_%d_%d" % (resstr, i, j)) not in blueskydict:
                    hires=False
                    break
            else:
                continue
            break

        if hires:
            output.log.debug("Photo: %s higher res is %s\n" % (name, resstr))
            return True
    return False
                

def makephoto(name, tex, lat, lon, scale, layer, pixels, output):
    # lat and lon are the NW corner cos that's how MSFS does it
    output.log.debug("Photo: %s %.6f,%.6f,%s " % (name, lat, lon, scale))
    if lon+LONRES*scale > floor(lon)+1:
        # Split EW
        output.log.debug("EW ")
        points=[[(Point(lat-LATRES*scale,lon),0,0),
                 (Point(lat-LATRES*scale,floor(lon)+1),(floor(lon)+1-lon)/(LONRES*scale),0),
                 (Point(lat,floor(lon)+1),(floor(lon)+1-lon)/(LONRES*scale),1),
                 (Point(lat,lon),0,1)],
                [(Point(lat-LATRES*scale,floor(lon)+1),(floor(lon)+1-lon)/(LONRES*scale),0),
                 (Point(lat-LATRES*scale,lon+(LONRES*scale)),1,0),
                 (Point(lat,lon+LONRES*scale),1,1),
                 (Point(lat,floor(lon)+1),(floor(lon)+1-lon)/(LONRES*scale),1)]]
    else:
        output.log.debug("OK ")
        points=[[(Point(lat-LATRES*scale,lon),0,0),	# SW
                 (Point(lat-LATRES*scale,lon+LONRES*scale),1,0),
                 (Point(lat,lon+LONRES*scale),1,1),	# NE
                 (Point(lat,lon),0,1)]]

    if lat>floor(lat-LATRES*scale)+1:
        output.log.debug("NS\n")
        # Split NS
        for i in range(len(points)):
            points.append([(Point(floor(lat-LATRES*scale)+1,points[i][3][0].lon),points[i][3][1],(floor(lat-LATRES*scale)+1-lat+LATRES*scale)/(LATRES*scale)),
                           (Point(floor(lat-LATRES*scale)+1,points[i][2][0].lon),points[i][2][1],(floor(lat-LATRES*scale)+1-lat+LATRES*scale)/(LATRES*scale)),
                           points[i][2], points[i][3]])
            points[i][2]=points[-1][1]
            points[i][3]=points[-1][0]
    else:
        output.log.debug("OK\n")

    poly=Polygon(name, tex, True, int(LATRES*1852*60*scale), layer, (lat-LATRES*scale*0.5, lon+LONRES*scale*0.5, pixels))
    output.polydat[name]=poly
    for p in points:
        output.polyplc.append((name, 65535, p))
