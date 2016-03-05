from convutil import viewer
from os import devnull
from os import mkdir
from os.path import basename
from os.path import dirname
from os.path import isdir
from os.path import join


class Log(object):
    def __init__(self, path, debug=False):
        if not isdir(dirname(path)):
            mkdir(dirname(path))
        self.logfile = file(path, 'at')
        self.logpath = path
        self.write_debug = debug
        self.debugpath = devnull
        if debug:
            self.debugpath = join(dirname(path), 'debug.txt')

        self.debugfile = file(self.debugpath, 'at')

    @property
    def file(self):
        return self.logfile

    @property
    def debug_file(self):
        return self.debugfile

    @property
    def debug_enabled(self):
        return self.write_debug

    @property
    def path(self):
        return self.logpath

    def is_logfile(self, path):
        return (basename(self.logpath) == path or
                basename(self.debugpath) == path)

    def close(self):
        self.logfile.close()
        self.debugfile.close()

    def view(self):
        self.logfile.flush()
        viewer(self.path)

    def debug(self, msg):
        if self.debug_enabled:
            self.debugfile.write('%s' % msg.encode('latin1', 'replace'))

    def info(self, msg):
        self.logfile.write('%s\n' % msg.encode('latin1', 'replace'))
