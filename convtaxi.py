#
# Copyright (c) 2006,2007 Jonathan Harris
# 
# Mail: <x-plane@marginal.org.uk>
# Web:  http://marginal.org.uk/x-planescenery/
#
# See FS2XPlane.html for usage.
#
# This software is licensed under a Creative Commons License
#   Attribution-ShareAlike 2.5:
#
#   You are free:
#     * to copy, distribute, display, and perform the work
#     * to make derivative works
#     * to make commercial use of the work
#   Under the following conditions:
#     * Attribution: You must give the original author credit.
#     * Share Alike: If you alter, transform, or build upon this work, you
#       may distribute the resulting work only under a license identical to
#       this one.
#   For any reuse or distribution, you must make clear to others the license
#   terms of this work.
#
# This is a human-readable summary of the Legal Code (the full license):
#   http://creativecommons.org/licenses/by-sa/2.5/legalcode
#

from math import sin, cos, pi, radians, degrees
from os.path import join
from sys import maxint

from OpenGL.GL import GL_LINE_LOOP, GL_TRUE
from OpenGL.GLU import *
try:
    # apparently older PyOpenGL version didn't define gluTessVertex
    gluTessVertex
except NameError:
    from OpenGL import GLU
    gluTessVertex = GLU._gluTessVertex

from convutil import AptNav, Point

twopi=pi+pi

# Runway, taxiway & helipad surfaces
surfaces={'ASPHALT':1, 'BITUMINOUS':1, 'MACADAM':1,
          'OIL_TREATED':1, 'TARMAC':1, 
          'BRICK':2, 'CEMENT':2, 'CONCRETE':2, 'STEEL_MATS':2,
          'GRASS':3,
          'DIRT':4, 'PLANKS':4,
          'SHALE':5, 'CORAL':5, 'GRAVEL':5,
          'CLAY':12, 'SAND':12,
          'WATER':13,
          'ICE':14, 'SNOW':14,
          'UNKNOWN':15}

designators={'C':'C', 'CENTER':'C', 'L':'L', 'LEFT':'L', 'R':'R', 'RIGHT':'R'}

# member var is defined
def D(c, v):
    return (v in dir(c) and eval("c.%s" % v))

# member var is defined and true
def T(c, v):
    return (v in dir(c) and eval("c.%s" % v)=='TRUE')

class Node:
    parking={'E_PARKING':'East ',  'NE_PARKING':'North East ',
             'N_PARKING':'North ', 'NW_PARKING':'North West ',
             'SE_PARKING':'South East ', 'S_PARKING':'South ',
             'SW_PARKING':'South West ', 'W_PARKING':'West ',
             'PARKING':''}
    types={'RAMP_CARGO':'Cargo ',
           'RAMP_GA':'GA ',        'RAMP_GA_LARGE':'GA ',
           'RAMP_GA_MEDIUM':'GA ', 'RAMP_GA_SMALL':'GA ',
           'RAMP_MIL_CARGO':'Mil cargo ', 'RAMP_MIL_COMBAT':'Military '}
    gates=['GATE_A', 'GATE_B', 'GATE_C', 'GATE_D', 'GATE_E', 'GATE_F',
           'GATE_G', 'GATE_H', 'GATE_I', 'GATE_J', 'GATE_K', 'GATE_L',
           'GATE_M', 'GATE_N', 'GATE_O', 'GATE_P', 'GATE_Q', 'GATE_R',
           'GATE_S', 'GATE_T', 'GATE_U', 'GATE_V', 'GATE_W', 'GATE_X',
           'GATE_Y', 'GATE_Z']

    def __init__(self, point):
        self.type=point.type
        if D(point, 'orientation') and point.orientation=='REVERSE':
            self.reverse=True
        else:
            self.reverse=False
        self.loc=Point(float(point.lat), float(point.lon))
        self.links=[]
        self.donecross=[[]]
        if D(point, 'name'):	# TaxiwayParking
            self.parking=True
        else:
            self.parking=False
        
        if self.parking and self.type not in ['VEHICLE','FUEL']:
            # Plane parking spot
            self.heading=float(point.heading)
            
            # type appears to be secondary to name
            if point.type in Node.gates:
                self.startup='Gate '+point.name[5]
            elif point.name=='GATE':
                self.startup='Gate'
            elif point.name=='DOCK':
                self.startup='Dock'
            elif point.name in Node.parking:
                self.startup=Node.parking[point.name]
                if point.type in Node.types:
                    self.startup+=Node.types[point.type]
                self.startup+='Ramp'
            # No name
            elif point.type.startswith('GATE'):
                self.startup='Gate'
            elif point.type=='DOC_GA':
                self.startup='Dock'
            elif point.type in Node.types:
                self.startup=Node.types[point.type]+'Ramp'
            # wtf
            else:
                self.startup='Ramp'
                
            if int(point.number):
                self.startup="%s %d" % (self.startup, int(point.number))
        else:
            self.startup=None


class Link:	# TaxiwayPath or TaxiwayParking
    def __init__(self, path, parkingoffset, taxinames):
        self.type=path.type
        self.width=float(path.width)
        if self.type!='PARKING':
            self.nodes=[int(path.start),int(path.end)]
        else:
            self.nodes=[int(path.start),int(path.end)+parkingoffset]
        if self.type=='RUNWAY':
            # We don't draw anything for runway paths
            self.draw=self.centreline=self.centrelights=False
            self.lines=['NONE','NONE']
            self.lights=[False,False]
            self.name='Runway '+path.number
            if path.designator in designators:
                self.name+=designators[path.designator]
        else:
            if self.type in ['PATH','PARKING']:
                self.draw=False	# No surface for apron paths
            else:
                self.draw=T(path, 'drawSurface') or T(path, 'drawDetail')
            self.centreline=T(path, 'centerLine')
            self.centrelights=T(path, 'centerLineLighted')
            self.lines=[path.rightEdge,path.leftEdge]
            self.lights=[T(path,'rightEdgeLighted'), T(path,'leftEdgeLighted')]
            self.name=self.type[0]+self.type[1:].lower()
            if taxinames[int(path.name)].name:
                self.name+=' '+taxinames[int(path.name)].name
        if self.draw:
            self.surface=surfaces[path.surface]
        else:
            self.surface=15
        self.done=0
        self.intersect=[[None,None],[None,None]]	# start right left, end right left

    def findlinked(self, searchspace, found):
        for n in [self.nodes[0],self.nodes[1]]:
            for link in n.links:
                if link.type=='RUNWAY':
                    break	# split graph at runways for easier editing
            else:
                for link in n.links:
                    if link.surface==self.surface and link in searchspace:
                        searchspace.remove(link)
                        found.append(link)
                        link.findlinked(searchspace, found)


# returns taxiway signs for debugging
def bezpt(out, this, next, ratio, brat, code, deb):
    brat=brat*ratio/3.0
    out.append(AptNav(112, "%10.6f %11.6f %10.6f %11.6f %s" % (
        this.lat+ratio*(next.lat-this.lat), this.lon+ratio*(next.lon-this.lon),
        this.lat+brat *(next.lat-this.lat), this.lon+brat *(next.lon-this.lon),
        code)))
    return [AptNav(20, "%10.6f %11.6f 0 0 5 {@R}%s" % (
        this.lat+ratio*(next.lat-this.lat), this.lon+ratio*(next.lon-this.lon),
        deb)),
            AptNav(20, "%10.6f %11.6f 0 0 5 {@Y}%s" % (
        this.lat+brat *(next.lat-this.lat), this.lon+brat *(next.lon-this.lon),
        deb))]


def edgefeature(link1, side1, link2=None, side2=None):
    if not link2:
        link2=link1
        side2=side1
    code=''
    if link1.type!='VEHICLE' and link2.type!='VEHICLE':
        if link1.lines[side1]=='SOLID' or link2.lines[side2]=='SOLID':
            code='53'
        elif link1.lines[side1]=='SOLID_DASHED' or link2.lines[side2]=='SOLID_DASHED':
            code='55'	# ?
        elif link1.lines[side1]=='DASHED' or link2.lines[side2]=='DASHED':
            code='59'
    else:	# vehicle
        if link1.lines[side1]=='SOLID' or link2.lines[side2]=='SOLID':
            code='20'
        elif link1.lines[side1]=='SOLID_DASHED' or link2.lines[side2]=='SOLID_DASHED':
            code='21'	# Use chequerboard
        elif link1.lines[side1]=='DASHED' or link2.lines[side2]=='DASHED':
            code='21'	# Displayed as chequerboard in MSFS
    if link1.lights[side1] or link2.lights[side2]:
        if code:
            code=code+' 102'
        else:
            code='102'
    return code


def follow(subgraph, ccw, tessObj, dump):

    # ccw is 1 or 0 (cw)
    if dump: print "Follow. CCW=", ccw, "#links=",len(subgraph)

    # Find nodes (this is more nodes than a convex hull)
    # start with westmost
    start=subgraph[0].nodes[0]	# a random node
    for link in subgraph:
        for n in [link.nodes[0],link.nodes[1]]:
            if n.loc.lon<start.loc.lon: start=n
    nodes=[start]
    
    n=nodes[0]
    lastheading=pi*1.5	# heading to last node
    #if dump: print n.loc, "node", lastheading
    while True:
        # Ack - O(n3) algorithm?!
        bestnode=None
        bestlink=None
        if not ccw and len(nodes)==1:
            bestheading=lastheading+twopi
        else:
            bestheading=lastheading-twopi
        #if not n.links: break	# node has no links - ignore
        for link in n.links:
            # find rightmost link
            if not link in subgraph:
                continue	# not this subgraph, or on spur
            for end in [0,1]:
                o=link.nodes[end]
                if o==n:
                    # We have a candidate
                    o=link.nodes[1-end]
                    h=n.loc.angleto(o.loc)
                    if h>=lastheading: h=h-twopi
                    #if dump: print o.loc, h
                    if not ccw and len(nodes)==1:
                        if h<bestheading:
                            bestnode=o
                            bestlink=link
                            bestheading=h
                    elif h>bestheading:
                        bestnode=o
                        bestlink=link
                        bestheading=h
                    break
        if len(nodes)>1 and n==nodes[0] and (not bestnode or bestnode==nodes[1]):
            # back at start and heading in the same direction
            nodes.pop()
            #if dump: print "(stop)"
            break
        if not bestnode:
            # On a spur - backtrack
            bestnode=nodes[nodes.index(n)-1]
        nodes.append(bestnode)
        lastheading=bestnode.loc.angleto(n.loc)
        #if dump: print bestnode.loc, "node", lastheading
        n=bestnode

    #if len(nodes)==1: continue	# node has no links - ignore

    # Go round nodes accumulating points
    points=[]
    bad=0	# is at least one point good?
    for nodeno in range(len(nodes)):
        n=nodes[nodeno]
        # This is only the exterior of _this_ surface - there might
        # be other exterior links. Arrange in order.
        elinks=[]
        lastheading=n.loc.angleto(nodes[(nodeno-1)%len(nodes)].loc)
        for link in n.links:
            # find links to the right
            for end in [0,1]:
                o=link.nodes[end]
                if o==n:
                    # We have a candidate
                    o=link.nodes[1-end]
                    h=n.loc.angleto(o.loc)
                    if h>lastheading: h=h-twopi
                    elinks.append((o,h,link,end))
                    break
        elinks.sort(lambda (o1,h1,l1,e1),(o2,h2,l2,e2): cmp(h2,h1)) # largest first
        if dump:
            print n.loc, "node, lastheading=", round(degrees(lastheading),2), len(elinks)
            for (n1,h1,link1,e1) in elinks:
                print n1.loc, round(degrees(h1),2)
                print link1.intersect[e1][1][0], link1.intersect[e1][0][0]

        if len(elinks)==1:	# Stub
            (n1,h1,link1,end1)=elinks[0]
            (loc1,bez1,bad1)=link1.intersect[end1][1]	# left
            (loc2,bez2,bad2)=link1.intersect[end1][0]	# right
            code2=edgefeature(link1, end1)
            if bad1: bad+=1
            if bad2: bad+=1
            if dump:
                print "%7.2f stub      %s" % (round(degrees(h1),2), loc1)
                print "%7.2f stub      %s" % (round(degrees(h1),2), loc2)
            #points.append((loc1, None, None, 0, ''))
            #points.append((loc2, None, None, 0, code2))
            points.append((loc1, loc1, None, None, 0, ''))
            points.append((loc2, loc2, None, None, 0, code2))
            link1.done+=1
            continue

        # Points at this node
        for i in range(len(elinks)):
            (n1,h1,link1,end1)=elinks[i]
            if n1==nodes[(nodeno+1)%len(nodes)]:
                link1.done+=1	# this is the next node
                if i: break	# stop if not also the first node
            # link to same (i!=0) or different surface
            (n2,h2,link2,end2)=elinks[(i+1)%len(elinks)]

            (int1,bez1,bad1)=link1.intersect[end1][1]	# ccw
            (int2,bez2,bad2)=link2.intersect[end2][0]	# 1-ccw
            loc1=int1
            loc2=int2
            code1=edgefeature(link1, 1-end1, link2, end2)
            code2=edgefeature(link2, end2)
            blank1=blank2=None
            if bad1: bad+=1
            if bad2: bad+=1

            # cases:
            # 1 link to runway
            # 2 link to apron path
            # 3 link to other surface (not a stub - stubs handled above)
            # 4 link to same surface
            
            # n1
            if dump: print "%7.2f" % round(degrees(h1),2),
            if not ccw and bad1:
                loc1=bez1=None
            elif bad1==2:
                loc1=None
                if dump: print "bad1      ",
            elif link1 not in subgraph and (link1.type=='RUNWAY' or link1.draw) and link2 not in subgraph and (link2.type=='RUNWAY' or link2.draw):
                loc1=n.loc	# 1: no fillets between runways
                bez1=None
                code1=''
                if dump: print "runways1  ",
            elif link1.type=='RUNWAY':
                blank1=True	# 1: end of blank across fillet
                if dump: print "endblank  ",
            elif not link1.draw and link1 not in subgraph:
                bez1=None	# 2: simplified pavement
                code1=''
                if dump: print "exit1     ",
            elif not link2.draw and link2.type!='RUNWAY' and link2 not in subgraph:
                loc1=bez1	# 2: start of simplified pavement
                bez1=None
                code1=''
                if dump: print "startexit ",
            elif link1 not in subgraph:
                if link2 in subgraph:
                    blank1=True		# 3: end of pavement cut
                    if dump: print "endcut    ",
                else:
                    loc1=None		# 3: middle of pavement cut
                    if dump: print "middlecut1",
            elif dump: print "normal1   ",

            if dump:
                if not loc1:
                    print "skip"
                elif bez1:
                    print bez1, "bez", int(degrees(h1-h2)%360)
                else:
                    print loc1

            # n2
            if dump: print "%7.2f" % round(degrees(h2),2),
            if not ccw and bad2:
                loc2=bez2=None
            elif bad2==2:
                loc2=None
                if dump: print "bad2      ",
            elif link2 not in subgraph and (link2.type=='RUNWAY' or link2.draw) and link1 not in subgraph and (link1.type=='RUNWAY' or link1.draw):
                loc2=n.loc	# 1: no fillets between runways
                bez2=None
                code2=''
                if dump: print "runways2  ",
            elif link2.type=='RUNWAY':
                blank2=False	# 1: start of blank across fillet
                if dump: print "startblank",
            elif not link2.draw and link2 not in subgraph:
                bez2=None	# 2: simplified pavement
                code2=''
                if dump: print "exit2     ",
            elif not link1.draw and link1.type!='RUNWAY' and link1 not in subgraph:
                loc2=bez2	# 2: end of simplified pavement
                bez2=None
                if dump: print "endexit   ",
            elif link2 not in subgraph:
                if link1 in subgraph:
                    blank2=False	# 3: start of pavement cut
                    if dump: print "startcut  ",
                else:
                    loc2=None		# 3: middle of pavement cut
                    if dump: print "middlecut2",
            elif dump: print "normal2   ",

            if dump:
                if not loc2:
                    print "skip"
                elif bez2:
                    print bez2, "bez", int(degrees(h1-h2)%360)
                else:
                    print loc2

            points.append((int1, loc1, bez1, blank1, bad1, code1))
            points.append((int2, loc2, bez2, blank2, bad2, code2))

    if dump:
        n=len(points)
        area2=0
        i=0
        while i<n:
            (int1, loc1, bez1, blank1, bad1, code1)=points[i]
            (int2, loc2, bez2, blank2, bad2, code2)=points[(i+1)%n]
            area2+=(int1.lon * int2.lat - int2.lon * int1.lat)
            i+=1
        if n==0:
            print "Poly", ccw, n, area2
            print "Empty poly!"
        else:
            print "Poly", ccw, n, area2, points[0][0]
            if (ccw and area2<=0) or (not ccw and area2>=0): print "Reversed poly!"
            if not ccw: print "goodness", len(points), bad

    # Skip this contour if empty or interior and >1/3 points bad
    if len(points)<3 or (not ccw and bad*2>len(points)):
        if dump and points: print "Skip poly:", len(points), bad, points[0][0]
        return

    # Feed points to tessellator, eliminating duplicates and adding
    # dummys to outscribe reflex curves
    if dump: print "Dummies:"
    gluTessBeginContour(tessObj)
    n=len(points)
    for i in range(0,n,2):
        # 1 and 2 are a pair of points at a node
        (int0, loc0, bez0, blank0, bad0, code0)=points[(i-1)%n]
        (int1, loc1, bez1, blank1, bad1, code1)=points[i]
        (int2, loc2, bez2, blank2, bad2, code2)=points[i+1]
        (int3, loc3, bez3, blank3, bad3, code3)=points[(i+2)%n]
        if not loc1:
            pass
        elif loc2 and not bez1 and not bez2 and loc1.equals(loc2):
            pass	# eg straight
        elif bad1==2:
            gluTessVertex(tessObj, [loc1.lon, 0, loc1.lat],
                          (loc1, None, None, bad1, code1))            
        elif bez1:
            cnt1=(bez1+(loc1-bez1)*(2.0/3)).round()
            gluTessVertex(tessObj, [bez1.lon, 0, bez1.lat],
                          (bez1, cnt1, blank1, 0, code1))
        elif loc1:
            gluTessVertex(tessObj, [loc1.lon, 0, loc1.lat],
                          (loc1, None, blank1, 0, code1))
        if bez1 or bez2:
            # dummy points to outscribe reflex curves
            h1=int1.angleto(int0)
            h2=int2.angleto(int3)
            if dump:
                print int0, int1, int3, round(degrees((h1-h2)%twopi),2)
                assert(int1.equals(int2))
            if (h1-h2)%twopi>pi:
                if bez1:
                    cnt1=(bez1+(loc1-bez1)*(2.0/3)).round()	# XXX
                    gluTessVertex(tessObj, [cnt1.lon, 0, cnt1.lat],
                                  (cnt1, None, None, 1, code1))	# dummy
                if bez2:
                    cnt2=(bez2+(loc2-bez2)*(2.0/3)).round()
                    gluTessVertex(tessObj, [cnt2.lon, 0, cnt2.lat],
                                  (cnt2, None, None, 1, code2))	# dummy
        if not loc2:
            pass
        elif loc3 and not bez2 and not bez3 and loc2.equals(loc3):
            pass	# eg at runway node
        elif bez2 and bez3 and abs(bez2.lat-bez3.lat)<=0.0000025 and abs(bez2.lon-bez3.lon)<=0.0000025:
            pass	# short link with shared bezier
        elif bad2==2:
            gluTessVertex(tessObj, [loc2.lon, 0, loc2.lat],
                          (loc2, None, None, bad2, code2))            
        elif bez2:
            cnt2=(bez2+(bez2-loc2)*(2.0/3)).round()
            gluTessVertex(tessObj, [bez2.lon, 0, bez2.lat],
                          (bez2, cnt2, blank2, 0, code2))
        elif loc2:
            gluTessVertex(tessObj, [loc2.lon, 0, loc2.lat],
                          (loc2, None, blank2, 0, code2))
    gluTessEndContour(tessObj)


# --------------------------------------------------------------------------

def tessbegin(datatype, data):
    if datatype!=GL_LINE_LOOP: raise GLUerror	# can't happen
    (points, debez, dump)=data
    points.append([])
    #if dump: print "Begin"
    
def tessend(data):
    (points, debez, dump)=data
    #if dump: print "End"

def tessvertex(vertex, data):
    (points, debez, dump)=data
    # just accumulate points
    points[-1].append(vertex)

def tesscombine(coords, vertex, weight, data):
    (points, debez, dump)=data
    if dump: print "Combine", Point(coords[2], coords[0])
    for i in range(len(weight)):
        if weight[i]:
            (loc, cnt, blank, dummy, code)=vertex[i]
            if dump:  print loc, cnt, blank, dummy, code, round(weight[i],3)
            if True:#weight[i]>0.1:	# arbitrary. Probably still OK if far away
                # Can no longer guarantee sanity of the bezier curve
                debez.append(loc)
    (loc, cnt, blank, dummy, code)=vertex[0]	# arbitrary - use first code
    return (Point(round(coords[2],6), round(coords[0],6)),
    #return (Point(coords[2], coords[0]),
            None, None, 0, code)


# --------------------------------------------------------------------------

def taxilayout(allnodes, alllinks, surfaceheading, output, aptdat=None, ident="unknown"):

    # Edges ----------------------------------------------------------------

    dump=output.debug
    if dump:
        print "Layout", ident
        nodesfile=open(join(output.xppath, ident+"_nodes.txt"), "at")
        interfile=open(join(output.xppath, ident+"_inter.txt"), "at")
        pointfile=open(join(output.xppath, ident+"_point.txt"), "at")

    # First find intersection & bezier points between every pair of links.
    # Then just join the dots.

    # First find all intersections
    for n in allnodes:
        elinks=[]
        for link in n.links:
            for end in [0,1]:
                o=link.nodes[end]
                if o!=n: continue
                o=link.nodes[1-end]
                h=n.loc.angleto(o.loc)
                elinks.append((o,h,link))
        elinks.sort(lambda (o1,h1,l1),(o2,h2,l2): cmp(h2,h1)) # CCW
        
        if dump:
            nodesfile.write("%.6f\t%.6f\n" % (n.loc.lon, n.loc.lat))
            print n.loc, "node", len(elinks), "links"

        for i in range(len(elinks)):
            (n1,h1,link1)=elinks[i]
            for end1 in [0,1]:
                if link1.nodes[end1]==n: break
            if len(elinks)==1:
                # Stub
                w=link1.width/2
                if link1.type=='RUNWAY':
                    loc1=n.loc.biased(cos(h1)*w, -sin(h1)*w).round()
                    loc2=n.loc.biased(-cos(h1)*w, sin(h1)*w).round()
                else:
                    loc1=n.loc.biased((cos(h1)-sin(h1))*w, (-sin(h1)-cos(h1))*w).round()
                    loc2=n.loc.biased((-cos(h1)-sin(h1))*w, (sin(h1)-cos(h1))*w).round()
                link1.intersect[end1]=[(loc1,None,0), (loc2,None,0)]
                if dump:
                    print loc1, "stub", i
                    print loc2, "stub", i
                    interfile.write("%.6f\t%.6f\n" % (loc1.lon, loc1.lat))
                    interfile.write("%.6f\t%.6f\n" % (loc2.lon, loc2.lat))
                break
            (n2,h2,link2)=elinks[(i+1)%len(elinks)]
            for end2 in [0,1]:
                if link2.nodes[end2]==n: break
            angle=(h1-pi-h2)%twopi
            if angle<0.1 or angle>twopi-0.1 or abs(h1-h2)<0.1:
                # straight - arbitrary ~5degrees
                w=(link1.width+link2.width)/4
                # not quite average but close enough
                intersect=n.loc.biased(-w*(cos(h1)+cos(h2-pi))/2,
                                       w*(sin(h1)+sin(h2-pi))/2).round()
                link1.intersect[end1][1]=(intersect,None,0)
                link2.intersect[end2][0]=(intersect,None,0)
                if dump: print intersect, "straight ", i, int(degrees(h1)), int(degrees(h2))
            else:
                ratio=0.5/sin((h1-pi)%twopi-h2)
                intersect=n.loc.biased(-sin(h1)*link2.width*ratio - sin(h2)*link1.width*ratio,
                                       -cos(h1)*link2.width*ratio - cos(h2)*link1.width*ratio).round()
                link1.intersect[end1][1]=(intersect,True,0)
                link2.intersect[end2][0]=(intersect,True,0)
                if dump: print intersect, "intersect", i, int(degrees(h1)), int(degrees(h2))
            if dump: interfile.write("%.6f\t%.6f\n" % (intersect.lon, intersect.lat))
            
    # Fill in bezier points. Can't do bezier control points yet
    if dump: print "Beziers"
    for link in alllinks:
        #if not link.intersect[0][0]: continue	# unlinked link!
        for end in [0,1]:
            (loc1,bez1,bad1)=link.intersect[end][0]
            (loc2,bez2,bad2)=link.intersect[1-end][1]
            # check whether intersections cross  ie link length is too short
            # for curves
            # http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d
            # p1=link.nodes[end], p2=loc1, p3=link.nodes[1-end], p4=loc2
            p1=link.nodes[end].loc
            p3=link.nodes[1-end].loc
            d=(loc2.lat-p3.lat)*(loc1.lon-p1.lon)-(loc2.lon-p3.lon)*(loc1.lat-p1.lat)
            if d==0:
                c=0
                a=b=5
            else:
                a=((loc2.lon-p3.lon)*(p1.lat-p3.lat)-(loc2.lat-p3.lat)*(p1.lon-p3.lon))/d
                b=((loc1.lon-p1.lon)*(p1.lat-p3.lat)-(loc1.lat-p1.lat)*(p1.lon-p3.lon))/d
            if a>=0 and a<=1 and b>=0 and b<=1:
                # link is too small and intersection points cross so beziers
                # will loop weirdly. Just let tessellator sort it out.
                d11=link.nodes[end].loc-loc1
                d12=link.nodes[end].loc-loc2
                d21=link.nodes[1-end].loc-loc1
                d22=link.nodes[1-end].loc-loc2
                bad1=max(bad1,1)
                bad2=max(bad2,1)
                bez1=bez2=None
                if d21.lat*d21.lat+d21.lon*d21.lon<=d22.lat*d22.lat+d22.lon*d22.lon:
                    bad1=2
                    bez1=None
                # mark other links' intersections with this link also bad
                for l2 in link.nodes[end].links:
                    for e2 in [0,1]:
                        if l2.nodes[e2]==link.nodes[end]:
                            for side in [0,1]:
                                if l2.intersect[e2][side][0].equals(loc1):
                                    l2.intersect[e2][side]=(loc1,None,bad1)
                if d11.lat*d11.lat+d11.lon*d11.lon>=d12.lat*d12.lat+d12.lon*d12.lon:
                    bad2=2
                    bez2=None
                # mark other links' intersections with this link also bad
                for l2 in link.nodes[1-end].links:
                    for e2 in [0,1]:
                        if l2.nodes[e2]==link.nodes[1-end]:
                            for side in [0,1]:
                                if l2.intersect[e2][side][0].equals(loc2):
                                    l2.intersect[e2][side]=(loc2,None,bad2)
                if dump:
                    print loc1, bez1, bad1, "degenerate1"
                    print loc2, bez2, bad2, "degenerate2"

            # eg KBJC
            d=loc1.distanceto(loc2)
            if (bez1 or bez2) and d<2.04*link.width:	# small fudge factor for safety
                # Would like to use a single point, but then
                # tessellator will merge in some circumstances (eg
                # adjacent taxiways off runway) and we lose the code.
                # So only make a single point if all adjacent links
                # are of the sdame surface -> will be in same pavement
                if bez1: bez1=(loc1+(loc2-loc1)*0.5).round()
                if bez2: bez2=(loc1+(loc2-loc1)*0.5).round()
                if bez1 and bez2:
                    h=loc1.angleto(loc2)
                    delta=Point(cos(h)*0.000001,sin(h)*0.000001).round()
                    bez1-=delta
                    bez2+=delta
                    for link2 in link.nodes[0].links:
                        if link2.type=='RUNWAY' or link2.surface!=link.surface: break
                    else:
                        for link2 in link.nodes[1].links:
                            if link2.type=='RUNWAY' or link2.surface!=link.surface: break
                        else:
                            bez1=(loc1+(loc2-loc1)*0.5).round()
                            bez2=(loc1+(loc2-loc1)*0.5).round()
                if dump:
                    print loc1, bez1, bad1, "shorten1"
                    print loc2, bez2, bad2, "shorten2"
            else:
                if bez1: bez1=(loc1+(loc2-loc1)*link.width/d).round()
                if bez2: bez2=(loc2+(loc1-loc2)*link.width/d).round()
                if dump:
                    print loc1, bez1, bad1
                    print loc2, bez2, bad2
                    
            if bez1 and abs(bez1.lat-loc1.lat)<0.00001 and abs(bez1.lon-loc1.lon)<0.00001:
                bez1=None	# collapses to a point
                #if dump: print "collapse1"
            link.intersect[end][0]=(loc1,bez1,bad1)
            if bez2 and abs(bez2.lat-loc2.lat)<0.00001 and abs(bez2.lon-loc2.lon)<0.00001:
                bez1=None	# collapses to a point
                #if dump: print "collapse2"
            link.intersect[1-end][1]=(loc2,bez2,bad2)
            if dump:
                if bez1:
                    pointfile.write("%.6f\t%.6f\n" % (bez1.lon, bez1.lat))
                else:
                    pointfile.write("%.6f\t%.6f\n" % (loc1.lon, loc1.lat))
                if bez2:
                    pointfile.write("%.6f\t%.6f\n" % (bez2.lon, bez2.lat))
                else:
                    pointfile.write("%.6f\t%.6f\n" % (loc2.lon, loc2.lat))
                    
    tessObj = gluNewTess()
    gluTessNormal(tessObj, 0, -1, 0)
    gluTessProperty(tessObj,GLU_TESS_WINDING_RULE,GLU_TESS_WINDING_NONZERO)
    #gluTessProperty(tessObj,GLU_TESS_WINDING_RULE,GLU_TESS_WINDING_POSITIVE)
    gluTessProperty(tessObj,GLU_TESS_BOUNDARY_ONLY,GL_TRUE)
    gluTessCallback(tessObj,GLU_TESS_BEGIN_DATA,   tessbegin)
    gluTessCallback(tessObj,GLU_TESS_VERTEX_DATA,  tessvertex)
    gluTessCallback(tessObj,GLU_TESS_END_DATA,     tessend)
    gluTessCallback(tessObj,GLU_TESS_COMBINE_DATA, tesscombine)

    # Candidates
    links=[l for l in alllinks if l.width>1 and (l.draw or l.lights[0] or l.lights[1] or l.lines[0]!='NONE' or l.lines[1]!='NONE') and (not aptdat or (not output.excluded(l.nodes[0].loc) and not output.excluded(l.nodes[1].loc)))]

    while links:
        newpoints=[]
        debez=[]
        gluTessBeginPolygon(tessObj, (newpoints, debez, dump))

        # Find an independent subgraph by looking for connected links
        subgraph=[links[0]]	# start with random link
        surface=links[0].surface
        if links[0].type=='TAXI':
            name="Taxiway"
        elif links[0].type=='CLOSED':
            name="Closed Taxiway"
        elif links[0].type=='VEHICLE':
            name="Road"
        else:
            name="Path"
        links.pop(0)
        subgraph[0].findlinked(links, subgraph)
        #if len(subgraph)<=1 and len(subgraph[0].links==0):
        #    continue	# orphan node - don't generate apt.dat entry

        # Exterior
        follow(subgraph, 1, tessObj, dump)

        # Interior(s)
        lastlen=maxint
        while True:
            interior=[link for link in subgraph if link.done<2]
            if not interior: break
            if len(interior)==lastlen: break	# prevent infloop on error
            follow(interior, 0, tessObj, dump)
            lastlen=len(interior)

        gluTessEndPolygon(tessObj)

        # Turn an intersected bezier curves in to straight lines
        for points in newpoints:
            for i in range(len(points)):
                (loc,cnt,blank,dummy,code)=points[i]
                if loc in debez or cnt in debez:
                    points[i]=(loc,None,None,dummy,code)

        # Tessellator spits out points in any order - first split out exteriors
        if dump: print "After tessellation:"
        outpoints=[]
        j=0
        while j<len(newpoints):
            points=newpoints[j]
            n=len(points)
            area2=0
            i=0
            while i<n:
                (loc,cnt,blank,dummy,code)=points[i]
                (loc1,cnt1,blank1,dummy1,code1)=points[(i+1)%n]
                area2+=(loc.lon * loc1.lat - loc1.lon * loc.lat)
                i+=1

            if area2>=0:	# exterior
                p=newpoints.pop(j)
                if area2>=1e-7:	# crappy exterior - arbitrary (eg ESSA, KLAS)
                    outpoints.append([p])
            elif area2>-1e-8:	# crappy interior - arbitrary
                newpoints.pop(j)
            else:
                j+=1
            if dump:
                if area2>=0: print "exterior %3d " % n, area2, area2>=1e-7
                else: print "interior %3d " % n, area2, area2<=-1e-8

        # Attach interiors to corresponding exterior. Assumes no intersections
        # http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/ (Soln 2)
        #if dump and newpoints: print "Assign interiors:"
        for e in outpoints:
            opoints=e[0]	# the exterior polygon
            n=len(opoints)
            #if dump: print "Exterior", len(opoints)
            j=0
            while j<len(newpoints):
                points=newpoints[j]	# candidate interior polygon
                for (loc,cnt,blank,dummy,code) in points:
                    #if dump: print len(points), j, loc,
                    angle=0
                    for i in range(n):
                        if loc.equals(opoints[i][0]):
                            # try another point in the candidate
                            #if dump: print "Coincident", loc,
                            break
                        # sum angles between exterior points
                        thisangle=loc.angleto(opoints[i][0])-loc.angleto(opoints[(i+1)%n][0])
                        if thisangle>pi: thisangle-=twopi
                        if thisangle<-pi: thisangle+=twopi
                        #print opoints[i][0], opoints[(i+1)%n][0], round(thisangle,6)
                        angle+=thisangle
                    else:
                        # angle is 0 (outside) or twopi (inside)
                        if angle>pi:
                            #if dump: print "Inside", angle
                            # interior polygon point is inside exterior
                            e.append(newpoints.pop(j))
                        else:
                            #if dump: print "Outside", angle
                            j+=1
                        break
                else:
                    # wtf - all interior points were coincident with exterior
                    #if dump: print "wtf", angle
                    newpoints.pop(j)
        if dump and newpoints:
            print "Unassigned interiors!"
            for points in newpoints: print len(points), points[0][0]
        
        # Finally output the polygons
        out=[]
        for tw in outpoints:
            out.append(AptNav(110, "%02d %4.2f %6.2f %s" % (
                surface, 0.25, surfaceheading, name)))
            for points in tw:
                n=len(points)
                if points[0][3]==1 and points[n-1][3]==1:	# dummy - see below
                    i=1
                else:
                    i=0
                while i<n:
                    (pt,bez,blank,dummy,code)=points[i]
                    i+=1
                    #if dummy==2: continue
                    if dummy==1 and points[i%n][3]==1:
                        i+=1 
                        continue
                    # if not one of a pair of dummies then an adjacent point
                    # must be inserted by a combine operation so make real
                    if blank==True:	# end of blank
                        out.append(AptNav(111, "%10.6f %11.6f" % (pt.lat, pt.lon)))
                    if bez:
                        out.append(AptNav(112, "%10.6f %11.6f %10.6f %11.6f %s" % (
                            pt.lat, pt.lon, bez.lat, bez.lon, code)))
                    else:
                        out.append(AptNav(111, "%10.6f %11.6f %s" % (pt.lat, pt.lon, code)))
                    if blank==False:	# start of blank
                        out.append(AptNav(111, "%10.6f %11.6f" % (pt.lat, pt.lon)))
                out[-1].code+=2		# Terminate last
        
        if aptdat:
            aptdat.extend(out)
        else:
            output.misc.append((110, subgraph[0].nodes[0].loc, out))

        #break	# XXX

    gluDeleteTess(tessObj)


    # Centre lines ---------------------------------------------------------

    # Candidates - have at least one link with a centre line or lights
    links=[l for l in alllinks if (l.centreline or l.centrelights) and (not aptdat or (not output.excluded(l.nodes[0].loc) and not output.excluded(l.nodes[1].loc)))]
    for n in allnodes:
        n.donecross=[[True for i in n.links] for i in n.links]
        if aptdat and output.excluded(n.loc): continue
        for i in range(len(n.links)):
            for j in range(len(n.links)):
                if i!=j and (n.links[i].centreline or n.links[i].centrelights):
                    if n.links[j].type=='RUNWAY':
                        for end in [0,1]:
                            if n!=n.links[j].nodes[end] and len(n.links[j].nodes[end].links)>1: break	# no cross for runway end
                        else:
                            continue
                    n.donecross[i][j]=n.donecross[j][i]=False

    # First do proper links, opportunistically adding crosses
    while links:
        stuff=[]

        # Look for a real end link - no other links at all from end node
        for link in links:
            for startend in [0,1]:
                if len(link.nodes[startend].links)==1:
                    break	# found one
            else:
                continue
            break
        else:
            # Look for an end link - no other valid links from end node
            for link in links:
                for startend in [0,1]:
                    for l in link.nodes[startend].links:
                        if link!=l and l in links and link.name==l.name and link.centreline==l.centreline and link.centrelights==l.centrelights:
                            break	# valid continuation - no good - goto A
                    else:
                        break	# found one - goto B
                else:
                    continue	# A
                break		# B
            else:
                # Only loops left - just pick first link
                link=links[0]
                startend=0

        if link.centreline:
            out=[AptNav(120,"Centreline for %s"  %link.name)]
            if link.type=='VEHICLE':
                code="22"
            else:
                code="51"
            if link.centrelights:
                code=code+" 101"
        else:
            out=[AptNav(120,"Centrelights for %s"%link.name)]
            code="101"

        # Control points are min(width, length/2)   along
        # Bezier  points are min(width, length/2)/3 along

        # Look for a cross before first node
        n=link.nodes[startend]	# start node
        i=n.links.index(link)
        for j in range(len(n.links)):
            if not n.donecross[i][j] and (link.centreline or not n.links[j].centreline) and (link.centrelights or not n.links[j].centrelights):
                n.donecross[i][j]=n.donecross[j][i]=True
                for end in [0,1]:
                    o=n.links[j].nodes[end]	# end node
                    if o!=n: break
                length=n.loc.distanceto(o.loc)
                if len(o.links)==1:
                    if length<link.width:
                        ctrl=1
                    else:
                        ctrl=link.width/length
                elif length>2*n.links[j].width:
                    ctrl=n.links[j].width/length
                else:
                    ctrl=0.5
                stuff.extend(bezpt(out, n.loc, o.loc, ctrl, 1,code,"0"))
                break

        # Follow path
        while True:
            n=link.nodes[startend]	# start node
            o=link.nodes[1-startend]	# next node
            length=n.loc.distanceto(o.loc)
            if len(n.links)==len(o.links)==1:
                out.append(AptNav(111, "%10.6f %11.6f %s" % (
                    n.loc.lat, n.loc.lon, code)))
                stuff.append(AptNav(20, "%10.6f %11.6f 0 0 5 %s" % (
                    n.loc.lat, n.loc.lon, "{@R}1")))
                out.append(AptNav(111, "%10.6f %11.6f %s" % (
                    o.loc.lat, o.loc.lon, code)))
                stuff.append(AptNav(20, "%10.6f %11.6f 0 0 5 %s" % (
                    o.loc.lat, o.loc.lon, "{@R}2")))
            elif len(n.links)==1:
                if length>link.width:
                    out.append(AptNav(111, "%10.6f %11.6f %s" % (
                        n.loc.lat, n.loc.lon, code)))
                    stuff.append(AptNav(20, "%10.6f %11.6f 0 0 5 %s" %(
                        n.loc.lat, n.loc.lon, "{@R}3")))
                stuff.extend(bezpt(out, o.loc, n.loc, min(1,link.width/length), 1, code, "4"))
            elif len(o.links)==1:
                stuff.extend(bezpt(out, n.loc, o.loc, min(1,link.width/length), 5, code, "5"))
                if length>link.width:
                    out.append(AptNav(111, "%10.6f %11.6f %s" % (
                        o.loc.lat, o.loc.lon, code)))
                    stuff.append(AptNav(20, "%10.6f %11.6f 0 0 5 %s" %(
                        o.loc.lat, o.loc.lon, "{@R}6")))
            elif length>2*link.width:
                stuff.extend(bezpt(out, n.loc, o.loc, link.width/length, 5, code, "7"))
                stuff.extend(bezpt(out, o.loc, n.loc, link.width/length, 1, code, "8"))
            else:
                stuff.extend(bezpt(out, n.loc, o.loc, 0.5, 5, code,"9"))
            links.remove(link)

            # Look for next link - greedy
            for l in o.links:
                if l in links and link.name==l.name and link.type==l.type and link.centreline==l.centreline and link.centrelights==l.centrelights:
                    i=o.links.index(link)
                    j=o.links.index(l)
                    o.donecross[i][j]=o.donecross[j][i]=True
                    link=l
                    for startend in [0,1]:
                        if link.nodes[startend]==o: break
                    break	# found one
            else:
                break	# didn't find one

        # Look for a cross at end node
        n=o		# start node
        i=n.links.index(link)
        for j in range(len(n.links)):
            if not n.donecross[i][j] and (link.centreline or not n.links[j].centreline) and (link.centrelights or not n.links[j].centrelights):
                n.donecross[i][j]=n.donecross[j][i]=True
                for end in [0,1]:
                    o=n.links[j].nodes[end]	# end node
                    if o!=n: break
                length=n.loc.distanceto(o.loc)
                if len(o.links)==1:
                    if length<link.width:
                        ctrl=1
                    else:
                        ctrl=link.width/length
                elif length>2*n.links[j].width:
                    ctrl=n.links[j].width/length
                else:
                    ctrl=0.5
                stuff.extend(bezpt(out, n.loc, o.loc, ctrl, 5,code,"-"))
                break

        # Terminate last
        if out[-1].code==111:
            out[-1].code=115
            out[-1].text=out[-1].text[:22]
        else:
            out[-1].code=116
            out[-1].text=out[-1].text[:45]
        if aptdat:
            aptdat.extend(out)
        else:
            output.misc.append((120, n.loc, out))

        if False:	# XXX debug
            stuff[0].text=stuff[0].text[:31]+'B'+stuff[0].text[32:]
            stuff[-1].text=stuff[-1].text[:31]+'L'+stuff[-1].text[32:]
            if aptdat:
                aptdat.extend(stuff)
            else:
                output.misc.append((120, n.loc, stuff))

    # Fill in rest of the crosses
    for n in allnodes:
        for i in range(len(n.links)):
            for j in range(len(n.links)):
                if n.donecross[i][j]:
                    continue
                stuff=[]
                n.donecross[i][j]=n.donecross[j][i]=True
                if n.links[i].centreline or n.links[j].centreline:
                    name="Centreline"
                    if n.links[i].type!='VEHICLE' and n.links[j].type!='VEHICLE':
                        code="51"
                    else:
                        code="22"
                    if n.links[i].centrelights or n.links[j].centrelights:
                        code=code+" 101"
                else:
                    name="Centrelights"
                    code="101"
                if n.links[i].name==n.links[j].name or n.links[j].name.startswith("Runway"):
                    out=[AptNav(120,"%s for %s"  % (name, n.links[i].name))]
                elif n.links[i].name.startswith("Runway"):
                    out=[AptNav(120,"%s for %s"  % (name, n.links[j].name))]
                else:
                    out=[AptNav(120,"%s for Taxi join" % name)]

                a=b=n
                for end in [0,1]:
                    if a==n: a=n.links[i].nodes[end]
                    if b==n: b=n.links[j].nodes[end]

                for (link,o,z) in [(n.links[i],a,1),(n.links[j],b,5)]:
                    length=n.loc.distanceto(o.loc)
                    if len(o.links)==1:
                        if length<link.width:
                            ctrl=1
                        else:
                            ctrl=link.width/length
                    elif length>2*link.width:
                        ctrl=link.width/length
                    else:
                        ctrl=0.5
                    stuff.extend(bezpt(out, n.loc, o.loc, ctrl, z, code, "X"))
                # Terminate last
                out[-1].code=116
                out[-1].text=out[-1].text[:45]
                if aptdat:
                    aptdat.extend(out)
                else:
                    output.misc.append((120, n.loc, out))

                if False:	# XXX debug
                    if aptdat:
                        aptdat.extend(stuff)
                    else:
                        output.misc.append((20, n.loc, stuff))


    # Startup and hold marks -----------------------------------------------
    for n in allnodes:
        if aptdat and output.excluded(n.loc): continue
        if n.type in ['HOLD_SHORT','ILS_HOLD_SHORT']:
            if not n.links: continue
            # Find a link that starts at this node
            for link in n.links:
                if link.nodes[0]==n:
                    h=n.loc.angleto(link.nodes[1].loc)
                    #if dump: print n.loc, n.type, "forward", h,
                    break
            else:	# No link starts at this node - reverse
                link=n.links[0]
                h=link.nodes[0].loc.angleto(n.loc)
                #if dump: print n.loc, n.type, "backward", h,
            out=[AptNav(120, "%s hold short" % link.name)]
            #if dump: print "reverse=", n.reverse
            if n.reverse: h=(h+pi)%twopi
            w=max(1, link.width/2-0.5)
            if n.type=='HOLD_SHORT':
                code="54"
            else:
                code="56 103"
            loc=n.loc.biased(-cos(h)*w, sin(h)*w)
            out.append(AptNav(111, "%10.6f %11.6f %s" % (
                loc.lat, loc.lon, code)))
            loc=n.loc.biased(cos(h)*w, -sin(h)*w)
            out.append(AptNav(115, "%10.6f %11.6f" % (
                loc.lat, loc.lon)))
            if aptdat:
                aptdat.extend(out)
            else:
                output.misc.append((120, n.loc, out))            
            
        elif n.parking:	# TaxiwayParking
            for link in n.links:
                if not link.centreline: continue
                if n.startup:
                    out=[AptNav(120, n.startup)]
                else:
                    out=[AptNav(120, link.name)]
                for end in [0,1]:
                    o=link.nodes[end]
                    if o!=n: break
                h=n.loc.angleto(o.loc)
                w=link.width/4
                if n.type=='VEHICLE':
                    code="22"	# not sure what to do
                else:
                    code="51"
                loc=n.loc.biased(-cos(h)*w, sin(h)*w)
                out.append(AptNav(111, "%10.6f %11.6f %s" % (
                    loc.lat, loc.lon, code)))
                loc=n.loc.biased(cos(h)*w, -sin(h)*w)
                out.append(AptNav(115, "%10.6f %11.6f" % (
                    loc.lat, loc.lon)))
                if aptdat:
                    aptdat.extend(out)
                else:
                    output.misc.append((120, n.loc, out))            
