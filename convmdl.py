from os.path import basename
from struct import unpack

from convbgl import findtex
from convutil import asciify, rgb2uv, Matrix, Object, Material, Texture


# handle FSX format library MDL file
class ProcScen:
    def __init__(self, bgl, enda, scale, libname, srcfile, texdir, output):
        # Old style scenery found and skipped
        self.old = False
        # Old style runways/roads found and skipped
        self.rrt = False
        # Animations found and skipped
        self.anim = False
        self.log = output.log

        # new-style objects are scaled when placed
        assert(scale == 1)

        comment = ("object %s in file %s"
                   % (libname, asciify(basename(srcfile), False)))

        self.tex = []
        self.mattex = []
        self.vt = []
        self.idx = []
        self.matrix = []
        self.amap = []
        self.scen = []
        self.data = {}
        self.maxlod = 0

        header = bgl.read(4)
        if header != 'RIFF':
            raise IOError('%s is not RIFF' % header)
        (mdlsize,) = unpack('<I', bgl.read(4))
        endmdl = bgl.tell()+mdlsize
        typ = bgl.read(4)
        if typ != 'MDLX':
            raise IOError('Unknown model type %s' % typ)
        while bgl.tell() < endmdl:
            c = bgl.read(4)
            (size,) = unpack('<I', bgl.read(4))
            if c == 'MDLD':
                end = size + bgl.tell()
                while bgl.tell() < end:
                    c = bgl.read(4)
                    (size,) = unpack('<I', bgl.read(4))
                    if c == 'TEXT':
                        self.read_text(bgl, size)
                    elif c == 'MATE':
                        self.read_mate(bgl, size, texdir, output.addtexdir,
                                       output.xpver)
                    elif c == 'INDE':
                        self.read_inde(bgl, size)
                    elif c == 'VERB':
                        self.read_verb(bgl, size)
                    elif c == 'TRAN':
                        self.read_tran(bgl, size)
                    elif c == 'AMAP':
                        self.read_amap(bgl, size)
                    elif c == 'SCEN':
                        self.read_scen(bgl, size)
                    elif c == 'LODT':
                        self.read_lodt(bgl, size)
                    else:
                        bgl.seek(size, 1)
            else:
                bgl.seek(size, 1)

        # objs by texture
        objs = {}
        # Only interested in highest LOD
        for (m, t, self.vt, self.idx, self.matrix) in self.data[self.maxlod]:
            self.log.debug("%s\n%s\n" % (t, self.matrix))
            objvt = []
            nrmmatrix = self.matrix.adjoint()
            if t:
                # Bunching scheme will need re-work
                assert not t.s and not t.n and not t.r
                for (x, y, z, nx, ny, nz, tu, tv) in self.vt:
                    (x, y, z) = self.matrix.transform(x, y, z)
                    (nx, ny, nz) = nrmmatrix.rotateAndNormalize(nx, ny, nz)
                    objvt.append((x, y, -z, nx, ny, -nz, tu, tv))
            else:
                # replace material with palette texture
                (pu, pv) = rgb2uv(m.d)
                for (x, y, z, nx, ny, nz, tu, tv) in self.vt:
                    (x, y, z) = self.matrix.transform(x, y, z)
                    (nx, ny, nz) = nrmmatrix.rotateAndNormalize(nx, ny, nz)
                    objvt.append((x, y, -z, nx, ny, -nz, pu, pv))
            if t in objs:
                obj = objs[t]
                if t and t.e:
                    # Because we don't compare on emissive
                    obj.tex.e = t.e
            else:
                objs[t] = obj = Object(libname, comment, t, None)
            obj.addgeometry(m, objvt, self.idx)

        # Add objs to library with one name
        if objs:
            output.objdat[libname] = objs.values()

    def read_text(self, bgl, size):
        self.tex.extend([bgl.read(64).strip('\0').strip()
                         for i in range(0, size, 64)])

    def read_mate(self, bgl, size, texdir, addtexdir, xpver):
        # http://www.fsdeveloper.com/wiki/index.php?title=MDL_file_format_(FSX)#MATE
        for i in range(0, size, 120):
            (flags1, flags2, diffuse, detail, normal, specular, emissive,
             reflection, fresnel, dr, dg, db, da, sr, sg, sb, sa, sp, ds,
             normalscale, recflectionscale, po, power, bloomfloor,
             ambientscale, srcblend, dstblend, alphafunc, alphathreshhold,
             zwritealpha) = unpack('<9I16f3I2f', bgl.read(120))
            # Get texture names
            diffuse = ((flags1 & Material.FSX_MAT_HAS_DIFFUSE) and
                       self.tex[diffuse] or None)
            emissive = ((flags1 & Material.FSX_MAT_HAS_EMISSIVE) and
                        self.tex[emissive] or None)
            if xpver <= 10:
                # Not supported in<=10, so no point doing lookup.
                normal = specular = reflection = None
            else:
                specular = ((flags1 & Material.FSX_MAT_HAS_SPECULAR) and
                            self.tex[specular] or None)
                normal = ((flags1 & Material.FSX_MAT_HAS_NORMAL) and
                          self.tex[normal] or None)
                reflection = ((flags1 & Material.FSX_MAT_HAS_REFLECTION) and
                              self.tex[reflection] or None)
            # Get texture filenames.
            if diffuse:
                diffuse = findtex(diffuse, texdir, addtexdir)
            if emissive:
                emissive = findtex(emissive, texdir, addtexdir)
            if specular:
                specular = findtex(specular, texdir, addtexdir)
            if normal:
                normal = findtex(normal, texdir, addtexdir)
            if reflection:
                reflection = findtex(reflection, texdir, addtexdir)

            t = ((diffuse or emissive) and
                 Texture(xpver, diffuse, emissive, specular, normal,
                         reflection) or None)
            m = Material(xpver, (dr, dg, db),
                         (flags1 & Material.FSX_MAT_SPECULAR) and
                         not specular and
                         ((sr != sg or sr != sb or sr < 0.9) and
                          (sr, sg, sb) != (0, 0, 0)) and
                         [sr, sg, sb] or None,
                         # Poly
                         False,
                         flags2 & Material.FSX_MAT_DOUBLE_SIDED != 0,
                         flags1 & Material.FSX_MAT_ZTEST_ALPHA and
                         alphafunc in [Material.FSX_MAT_ALPHA_TEST_GREATER,
                                       Material.FSX_MAT_ALPHA_TEST_GREATER_EQUAL] and
                         alphathreshhold / 255 or None,
                         not diffuse and
                         ((flags1 & Material.FSX_MAT_SPECULAR) and
                          (sr, sg, sb) != (0, 0, 0)) and True,
                         flags1 & Material.FSX_MAT_NO_SHADOW != 0)
            self.mattex.append((m, t))
        self.log.debug("Materials %d\n" % len(self.mattex))
        for i in range(len(self.mattex)):
            self.log.debug("%3d:\t%s\t%s\n"
                           % (i, self.mattex[i][0], self.mattex[i][1]))

    def read_inde(self, bgl, size):
        self.idx = unpack('<%dH' % (size / 2), bgl.read(size))

    def read_verb(self, bgl, size):
        endv = size + bgl.tell()
        while bgl.tell() < endv:
            c = bgl.read(4)
            (size,) = unpack('<I', bgl.read(4))
            if c == 'VERT':
                self.vt.append([unpack('<8f', bgl.read(32))
                                for i in range(0, size, 32)])
            else:
                bgl.seek(size, 1)

    def read_tran(self, bgl, size):
        for i in range(0, size, 64):
            self.matrix.append(Matrix([unpack('<4f', bgl.read(16))
                                       for j in range(4)]))
        self.log.debug("Matrices %d\n" % len(self.matrix))
        for i in range(len(self.matrix)):
            self.log.debug("%s = %d\n" % (self.matrix[i], i))

    def read_amap(self, bgl, size):
        for i in range(0, size, 8):
            (a, b) = unpack('<2I', bgl.read(8))
            self.amap.append(b)
            self.log.debug("Animation map %d\n" % len(self.amap))
            for i in range(len(self.amap)):
                self.log.debug("%2d: %2d\n" % (i, self.amap[i]))

    def read_scen(self, bgl, size):
        # Assumed to be after TRAN and AMAP sections
        count = size / 8
        for i in range(count):
            (child, peer, offset, unk) = unpack('<4h', bgl.read(8))
            self.scen.append((child, peer, offset, -1))
        # Invert Child/Peer pointers to get parents
        for i in range(count):
            (child, peer, thisoff, parent) = self.scen[i]
            if child != -1:
                # child's parent is me
                (xchild, xpeer, xoff, xparent) = self.scen[child]
                self.scen[child] = (xchild, xpeer, xoff, i)
            if peer != -1:
                # peer's parent is my parent
                (xchild, xpeer, xoff, xparent) = self.scen[peer]
                self.scen[peer] = (xchild, xpeer, xoff, parent)
        # Replace AMAP offsets with matrix
        self.log.debug("Scene Graph %d\n" % len(self.scen))
        for i in range(count):
            (child, peer, offset, parent) = self.scen[i]
            self.scen[i] = (child, peer, self.matrix[self.amap[offset / 8]],
                            parent)
            self.log.debug("%2d: %2d %2d %2d %2d\n"
                           % (i, child, peer, parent, offset / 8))

    def read_lodt_lode_part(self, bgl, size, lod):
        (typ, scene, material, verb, voff, vcount, ioff,
         icount, unk) = unpack('<9I', bgl.read(36))
        assert (typ == 1)
        self.maxlod = max(lod, self.maxlod)
        (child, peer, finalmatrix, parent) = self.scen[scene]
        self.log.debug("LOD %4d: scene %d verb %d material %d tris %d voff %d "
                       "vcount %d ioff %d icount %d\n"
                       % (lod, scene, verb, material, icount / 3, voff, vcount,
                          ioff, icount))

        while parent != -1:
            (child, peer, thismatrix,
             parent) = self.scen[parent]
            finalmatrix = finalmatrix*thismatrix
        if lod not in self.data:
            self.data[lod] = []
        self.data[lod].append((self.mattex[material][0],
                               self.mattex[material][1],
                               self.vt[verb][voff:voff+vcount],
                               self.idx[ioff:ioff+icount],
                               finalmatrix))

    def read_lodt_lode(self, bgl, size):
        ende = size + bgl.tell()
        (lod,) = unpack('<I', bgl.read(4))
        while bgl.tell() < ende:
            c = bgl.read(4)
            (size,) = unpack('<I', bgl.read(4))
            if c == 'PART':
                self.read_lodt_lode_part(bgl, size, lod)
            else:
                bgl.seek(size, 1)

    def read_lodt(self, bgl, size):
        endt = size + bgl.tell()
        while bgl.tell() < endt:
            c = bgl.read(4)
            (size,) = unpack('<I', bgl.read(4))
            if c == 'LODE':
                self.read_lodt_lode(bgl, size)
            else:
                bgl.seek(size, 1)
