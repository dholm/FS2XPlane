from convutil import viewer
from os import mkdir
from os.path import isdir
from os.path import dirname


class Log(object):
    def __init__(self, path):
        if not isdir(dirname(path)):
            mkdir(dirname(path))
        self.logfile = file(path, 'at')
        self.logpath = path

    @property
    def file(self):
        return self.logfile

    @property
    def path(self):
        return self.logpath

    def close(self):
        self.logfile.close()

    def view(self):
        self.logfile.flush()
        viewer(self.path)

    def write(self, msg):
        self.logfile.write('%s\n' % msg.encode('latin1', 'replace'))
