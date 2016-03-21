from chunk import Chunk
from os.path import basename
from struct import unpack

from convbgl import ProcScen as ProcBgl
from convbgl import findtex
from convutil import asciify, rgb2uv, Matrix, Object, Material, Texture


class Riff(Chunk, object):
    def __init__(self, fil):
        super(Riff, self).__init__(fil, align=False, bigendian=False,
                                   inclheader=False)


class ProcMdl(object):
    def __init__(self, output):
        self.log = output.log

    def skip_chunk(self, section, chunk):
        if len(chunk.getname()):
            self.log.debug("Skipping %s chunk %r (%d bytes)..\n"
                           % (section, chunk.getname(), chunk.getsize()))
            chunk.skip()


class ProcMdlx(ProcMdl, object):
    def __init__(self, mdlx, scale, libname, texdir, output, comment):
        super(ProcMdlx, self).__init__(output)
        # Old style scenery found and skipped
        self.old = False
        # Old style runways/roads found and skipped
        self.rrt = False
        # Animations found and skipped
        self.anim = False

        # new-style objects are scaled when placed
        assert(scale == 1)

        self.tex = []
        self.mattex = []
        self.vt = []
        self.idx = []
        self.matrix = []
        self.amap = []
        self.scen = []
        self.data = {}
        self.maxlod = 0

        self.log.debug("Parsing MDLX\n")
        mdld_parser = {
            'TEXT': lambda chunk: self.read_text(chunk),
            'MATE': lambda chunk: self.read_mate(chunk, texdir,
                                                 output.addtexdir,
                                                 output.xpver),
            'INDE': lambda chunk: self.read_inde(chunk),
            'VERB': lambda chunk: self.read_verb(chunk),
            'TRAN': lambda chunk: self.read_tran(chunk),
            'AMAP': lambda chunk: self.read_amap(chunk),
            'SCEN': lambda chunk: self.read_scen(chunk),
            'LODT': lambda chunk: self.read_lodt(chunk)
        }
        while mdlx.tell() < mdlx.getsize():
            mdld = Riff(mdlx)
            if mdld.getname() == 'MDLD':
                while mdld.tell() < mdld.getsize():
                    chunk = Riff(mdld)
                    skip = lambda chunk: self.skip_chunk('MDLD', chunk)
                    parse = mdld_parser.get(chunk.getname(), skip)
                    parse(chunk)

            elif len(mdld.getname()):
                self.skip_chunk('MDLX', mdld)

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

    def read_text(self, chunk):
        self.tex.extend([chunk.read(64).decode('windows-1250').strip(' \0')
                         for i in range(0, chunk.getsize(), 64)])

    def read_mate(self, chunk, texdir, addtexdir, xpver):
        # http://www.fsdeveloper.com/wiki/index.php?title=MDL_file_format_(FSX)#MATE
        for i in range(0, chunk.getsize(), 120):
            (flags1, flags2, diffuse, detail, normal, specular, emissive,
             reflection, fresnel, dr, dg, db, da, sr, sg, sb, sa, sp, ds,
             normalscale, recflectionscale, po, power, bloomfloor,
             ambientscale, srcblend, dstblend, alphafunc, alphathreshhold,
             zwritealpha) = unpack('<9I16f3I2f', chunk.read(120))
            # Get texture names
            diffuse = ((flags1 & Material.FSX_MAT_HAS_DIFFUSE) and
                       self.tex[diffuse] or None)
            emissive = ((flags1 & Material.FSX_MAT_HAS_EMISSIVE) and
                        self.tex[emissive] or None)
            self.log.debug("\tmat f1: %08x f2: %08x sblnd: %x dblnd: %x "
                           "afun: %x\n"
                           % (flags1, flags2, srcblend, dstblend, alphafunc))
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
                         flags1 & Material.FSX_MAT_NO_SHADOW != 0,
                         (srcblend, dstblend))
            self.mattex.append((m, t))
        self.log.debug("Materials %d\n" % len(self.mattex))
        for i in range(len(self.mattex)):
            self.log.debug("%3d:\t%s\t%s\n"
                           % (i, self.mattex[i][0], self.mattex[i][1]))

    def read_inde(self, chunk):
        self.idx = unpack('<%dH' % (chunk.getsize() / 2),
                          chunk.read(chunk.getsize()))

    def read_verb(self, chunk):
        endv = chunk.getsize() + chunk.tell()
        while chunk.tell() < endv:
            c = Riff(chunk)
            if c.getname() == 'VERT':
                self.vt.append([unpack('<8f', c.read(32))
                                for i in range(0, c.getsize(), 32)])
            elif len(c.getname()):
                c.skip()

    def read_tran(self, chunk):
        for i in range(0, chunk.getsize(), 64):
            self.matrix.append(Matrix([unpack('<4f', chunk.read(16))
                                       for j in range(4)]))
        self.log.debug("Matrices %d\n" % len(self.matrix))
        for i in range(len(self.matrix)):
            self.log.debug("%s = %d\n" % (self.matrix[i], i))

    def read_amap(self, chunk):
        for i in range(0, chunk.getsize(), 8):
            (a, b) = unpack('<2I', chunk.read(8))
            self.amap.append(b)
            self.log.debug("Animation map %d\n" % len(self.amap))
            for i in range(len(self.amap)):
                self.log.debug("%2d: %2d\n" % (i, self.amap[i]))

    def read_scen(self, chunk):
        # Assumed to be after TRAN and AMAP sections
        count = chunk.getsize() / 8
        for i in range(count):
            (child, peer, offset, unk) = unpack('<4h', chunk.read(8))
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

    def read_lodt_lode_part(self, chunk, lod):
        (typ, scene, material, verb, voff, vcount, ioff,
         icount, unk) = unpack('<9I', chunk.read(36))
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

    def read_lodt_lode(self, chunk):
        ende = chunk.getsize() + chunk.tell()
        (lod,) = unpack('<I', chunk.read(4))
        while chunk.tell() < ende:
            c = Riff(chunk)
            if c.getname() == 'PART':
                self.read_lodt_lode_part(c, lod)
            elif len(c.getname()):
                self.log.debug("Skipping lodt lode chunk %r (%d bytes)..\n"
                               % (c.getname(), c.getsize()))
                c.skip()

    def read_lodt(self, chunk):
        endt = chunk.getsize() + chunk.tell()
        while chunk.tell() < endt:
            c = Riff(chunk)
            if c.getname() == 'LODE':
                self.read_lodt_lode(c)
            elif len(c.getname()):
                self.log.debug("Skipping lodt chunk %r (%d bytes)..\n"
                               % (c.getname(), c.getsize()))
                c.skip()


class ProcMdl89(ProcMdl, object):
    def __init__(self, mdl89, scale, libname, srcfile, texdir, output,
                 comment):
        super(ProcMdl89, self).__init__(output)
        # Old style scenery found and skipped
        self.old = False
        # Old style runways/roads found and skipped
        self.rrt = False
        # Animations found and skipped
        self.anim = False

        self.log.debug("Parsing MDL8/9\n")
        bgl = ProcBgl(scale, libname, srcfile, texdir, output)
        parse_bgl = lambda chunk: bgl.parse(chunk,
                                            chunk.getsize() - chunk.tell())
        exte_parser = {
            'TEXT': parse_bgl,
            'MATE': parse_bgl,
            'VERT': parse_bgl,
            'BGL ': parse_bgl
        }
        while mdl89.tell() < mdl89.getsize():
            riff = Riff(mdl89)
            if riff.getname() == 'EXTE':
                while riff.tell() < riff.getsize():
                    chunk = Riff(riff)
                    startp = riff.tell()
                    skip = lambda chunk: self.skip_chunk('EXTE', chunk)
                    parse = exte_parser.get(chunk.getname(), skip)
                    parse(chunk)
                    endp = riff.tell()
                    if endp - startp < chunk.getsize():
                        self.log.debug('Chunk %s ended after %d/%d bytes.\n'
                                       % (chunk.getname(), endp - startp,
                                          chunk.getsize()))
                        chunk.skip()

            elif len(riff.getname()):
                self.skip_chunk('MDL89', riff)


# handle FSX format library MDL file
def ProcScen(bgl, enda, scale, libname, srcfile, texdir, output):
    comment = ("object %s in file %s"
               % (libname, asciify(basename(srcfile), False)))

    riff = Riff(bgl)
    if riff.getname() != 'RIFF':
        raise IOError('%s is not RIFF' % riff.getname())
    typ = riff.read(4)
    if typ == 'MDLX':
        return ProcMdlx(riff, scale, libname, texdir, output, comment)
    elif typ == 'MDL8' or typ == 'MDL9':
        return ProcMdl89(riff, scale, libname, srcfile, texdir, output,
                         comment)
    else:
        raise IOError('Unknown model type %s' % typ)
