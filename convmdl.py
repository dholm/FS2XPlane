from chunk import Chunk
from os.path import basename
from struct import unpack
from uuid import UUID

from convbgl import ProcScen as ProcBgl
from convbgl import findtex
from convutil import asciify, rgb2uv, Matrix, Object, Material, Texture


class Riff(Chunk, object):
    def __init__(self, fil):
        super(Riff, self).__init__(fil, align=False, bigendian=False,
                                   inclheader=False)

    def _read_unpack(self, fmt, size, count):
        d = unpack('<%d%c' % (count, fmt), self.read(size * count))
        if count == 1:
            return d[0]
        return d

    def read_uint8(self, count=1):
        return self._read_unpack('B', 1, count)

    def read_uint16(self, count=1):
        return self._read_unpack('H', 2, count)

    def read_int16(self, count=1):
        return self._read_unpack('h', 2, count)

    def read_uint32(self, count=1):
        return self._read_unpack('I', 4, count)

    def read_float(self, count=1):
        return self._read_unpack('f', 4, count)

    def read_string(self, size=-1):
        s = ''
        if size == -1:
            while True:
                c = self.read(1)
                if c == '\0':
                    break
                s += c

        else:
            s = self.read(size).decode('windows-1250')

        return s.strip(' \0')


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

        self.name = None
        self.guid = None
        self.tex = []
        self.mattex = []
        self.vt = []
        self.idx = []
        self.matrix = []
        self.amap = []
        self.alnk = []
        self.alst = {}
        self.scen = []
        self.data = {}
        self.bmap = {}
        self.blnk = []
        self.jcon = []
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
            'SGAL': lambda chunk: self.read_sgal(chunk),
            'SCEN': lambda chunk: self.read_scen(chunk),
            'LODT': lambda chunk: self.read_lodt(chunk),
            'PLAL': lambda chunk: self.read_plal(chunk),
            'ANIB': lambda chunk: self.read_anib(chunk),
            'BMAP': lambda chunk: self.read_bmap(chunk),
            'SGBR': lambda chunk: self.read_sgbr(chunk),
            'SGJC': lambda chunk: self.read_sgjc(chunk),
        }
        while mdlx.tell() < mdlx.getsize():
            mdl = Riff(mdlx)
            if mdl.getname() == 'MDLD':
                while mdl.tell() < mdl.getsize():
                    chunk = Riff(mdl)
                    skip = lambda chunk: self.skip_chunk('MDLD', chunk)
                    parse = mdld_parser.get(chunk.getname(), skip)
                    self.log.debug("MDLD chunk %r (%d bytes)..\n"
                                   % (chunk.getname(), chunk.getsize()))
                    parse(chunk)

            elif mdl.getname() == 'MDLG':
                self.read_guid(mdl)
            elif mdl.getname() == 'MDLN':
                self.read_name(mdl)
            elif len(mdl.getname()):
                self.skip_chunk('MDLX', mdl)

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

    def read_guid(self, chunk):
        self.guid = UUID(bytes=chunk.read(chunk.getsize()))
        self.log.debug('GUID %s\n' % self.guid)

    def read_name(self, chunk):
        self.name = chunk.read_string(chunk.getsize())
        self.log.debug('Name %s\n' % self.name)

    def read_text(self, chunk):
        self.tex.extend([chunk.read_string(64)
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
        self.idx = chunk.read_uint16(chunk.getsize() / 2)

    def read_verb(self, chunk):
        endv = chunk.getsize() + chunk.tell()
        while chunk.tell() < endv:
            c = Riff(chunk)
            if c.getname() == 'VERT':
                self.vt.append([c.read_float(8)
                                for i in range(0, c.getsize(), 32)])
            elif len(c.getname()):
                c.skip()

    def read_tran(self, chunk):
        for i in range(0, chunk.getsize(), 64):
            self.matrix.append(Matrix([chunk.read_float(4)
                                       for j in range(4)]))
        self.log.debug("Matrices %d\n" % len(self.matrix))
        for i in range(len(self.matrix)):
            self.log.debug("%s = %d\n" % (self.matrix[i], i))

    def read_amap(self, chunk):
        for i in range(0, chunk.getsize(), 8):
            (a, b) = chunk.read_uint32(2)
            self.amap.append((a, b))
            self.log.debug("Animation map %d\n" % len(self.amap))
            for i in range(len(self.amap)):
                typ = self.amap[i][0]
                typ = ('static' if typ == 1 else
                       'animated' if typ == 2 else
                       str(typ))
                self.log.debug("%2d: %2d (%s)\n" % (i, self.amap[i][1], typ))

    def read_bmap(self, chunk):
        bmap_idx = chunk.read_uint32()
        for i in range(0, chunk.getsize(), 4):
            sgbr_idx = chunk.read_uint32()
            self.bmap[i] = sgbr_idx

    def read_sgbr(self, chunk):
        count = chunk.getsize() / 2
        for _ in range(count):
            bone_id = chunk.read_uint16()
            self.blnk.append(bone_id)

    def read_sgjc(self, chunk):
        count = chunk.getsize() / 2
        for _ in range(count):
            joint_constraint = chunk.read_uint16()
            self.jcon.append(joint_constraint)

    def read_scen(self, chunk):
        # Assumed to be after TRAN and AMAP sections
        count = chunk.getsize() / 8
        for i in range(count):
            (child, peer, offset, unk) = chunk.read_int16(4)
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
            self.scen[i] = (child, peer, self.matrix[self.amap[offset / 8][1]],
                            parent)
            self.log.debug("%2d: %2d %2d %2d %2d\n"
                           % (i, child, peer, parent, offset / 8))

    def read_part(self, chunk, lod):
        (typ, scene, material, verb, voff, vcount, ioff,
         icount, unk) = chunk.read_uint32(9)
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

    def read_lode(self, chunk):
        ende = chunk.getsize() + chunk.tell()
        lod = chunk.read_uint32()
        while chunk.tell() < ende:
            c = Riff(chunk)
            if c.getname() == 'PART':
                self.read_part(c, lod)
            elif len(c.getname()):
                self.skip_chunk('LODE', c)

    def read_lodt(self, chunk):
        endt = chunk.getsize() + chunk.tell()
        while chunk.tell() < endt:
            c = Riff(chunk)
            if c.getname() == 'LODE':
                self.read_lode(c)
            elif len(c.getname()):
                self.skip_chunk('LODT', c)

    def read_plat(self, chunk):
        surface = chunk.read_uint32()
        sgref = chunk.read_uint32()
        count = chunk.read_uint32()
        vtx = []
        for i in range(count):
            vtx.append(chunk.read_float(3))

    def read_plal(self, chunk):
        endv = chunk.getsize() + chunk.tell()
        while chunk.tell() < endv:
            c = Riff(chunk)
            if c.getname() == 'PLAT':
                self.read_plat(c)

            elif len(c.getname()):
                self.skip_chunk('PLAL', c)

    def read_xank(self, chunk, animation):
        typ = chunk.read_uint8()
        time = chunk.read_float()
        if typ == 4:
            # Rotation
            (q0, q1, q2, q3) = chunk.read_float(4)
            animation.append((time, q0, q1, q2, q3))
            self.log.debug('\t\t\t\tXANK rotation (%f, %f, %f, %f) '
                           '(%d bytes)\n'
                           % (q0, q1, q2, q3, chunk.getsize()))
        elif typ == 3:
            # Translation
            (x, y, z) = chunk.read_float(3)
            animation.append((time, x, y, z))
            self.log.debug('\t\t\t\tXANK translation (%d) (%f, %f, %f) '
                           '(%d bytes)\n'
                           % (typ, x, y, z, chunk.getsize()))
        else:
            raise IOError('Unsupported XANK type %d!' % typ)

    def read_xans(self, chunk):
        endv = chunk.getsize() + chunk.tell()
        (typ, anim_id) = chunk.read_uint32(2)
        length = chunk.read_float()

        self.log.debug('\t\t\tXANS type %d id %d length %f (%d bytes)..\n'
                       % (typ, anim_id, length, chunk.getsize()))
        animation = []
        while chunk.tell() < endv:
            c = Riff(chunk)
            if c.getname() == 'XANK':
                self.read_xank(c, animation)
            else:
                self.skip_chunk('XANS', c)

        if anim_id not in self.alst:
            self.alst[anim_id] = [animation]
        self.alst[anim_id].append(animation)

    def read_xani(self, chunk):
        endv = chunk.getsize() + chunk.tell()
        guid = UUID(bytes=chunk.read(16))
        length = chunk.read_float()
        typ = chunk.read_string(16)
        param = chunk.read_string()
        self.log.debug('\t\tXANI GUID %s length %f type %s param %s\n'
                       % (guid, length, typ, param))
        while chunk.tell() < endv:
            c = Riff(chunk)
            if c.getname() == 'XANS':
                self.read_xans(c)
            else:
                self.skip_chunk('XANI', c)

    def read_xanl(self, chunk):
        endv = chunk.getsize() + chunk.tell()
        self.log.debug('\tXANL (%d bytes)..\n' % chunk.getsize())
        while chunk.tell() < endv:
            c = Riff(chunk)
            if c.getname() == 'XANI':
                self.read_xani(c)
            else:
                self.skip_chunk('XANL', c)

    def read_anib(self, chunk):
        riff = Riff(chunk)
        endv = riff.getsize() + riff.tell()
        while riff.tell() < endv:
            c = Riff(riff)
            if c.getname() == 'XANL':
                self.read_xanl(c)
            else:
                self.skip_chunk('ANIB', c)

        chunk.skip()

    def read_sgal(self, chunk):
        count = chunk.getsize() / 2
        for i in range(count):
            anim_id = chunk.read_uint16()
            self.alnk.append(anim_id)


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
