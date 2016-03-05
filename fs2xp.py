#!/usr/bin/python

#
# Copyright (c) 2006,2007 Jonathan Harris
#
# Mail: <x-plane@marginal.org.uk>
# Web:  http://marginal.org.uk/x-planescenery/
#
# See FS2XPlane.html for usage.
#
# This software is licensed under a Creative Commons License
#   Attribution-Noncommercial-Share Alike 3.0:
#
#   You are free:
#    * to Share - to copy, distribute and transmit the work
#    * to Remix - to adapt the work
#
#   Under the following conditions:
#    * Attribution. You must attribute the work in the manner specified
#      by the author or licensor (but not in any way that suggests that
#      they endorse you or your use of the work).
#    * Noncommercial. You may not use this work for commercial purposes.
#    * Share Alike. If you alter, transform, or build upon this work,
#      you may distribute the resulting work only under the same or
#      similar license to this one.
#
#   For any reuse or distribution, you must make clear to others the
#   license terms of this work.
#
# This is a human-readable summary of the Legal Code (the full license):
#   http://creativecommons.org/licenses/by-nc-sa/3.0/
#

import argparse

from os import chdir
from os.path import (abspath, normpath, basename, dirname, pardir, exists,
                     isdir, join)
from sys import exit, argv
from traceback import print_exc

from convmain import Output
from convutil import FS2XError
from log import Log


# callbacks
def status(percent, msg):
    if percent < 0:
        print
    print msg.encode('latin1', 'replace')


def refresh():
    pass


def parse_args():
    class store_season(argparse.Action):
        Choices = ['spring', 'summer', 'autumn', 'winter']

        def __call__(self, parser, namespace, season, option_string=None):
            season_idx = self.Choices.index(season)
            setattr(namespace, self.dest, season_idx)

    parser = argparse.ArgumentParser()
    parser.add_argument('msfs_scenery_path',
                        help='Path to MS Flight Simulator scenery.')
    parser.add_argument('xplane_scenery_path',
                        help='Path to generated X-Plane scenery.')
    parser.add_argument('-s', '--season', default=0,
                        choices=store_season.Choices, action=store_season,
                        help='Season to convert to X-Plane.')
    parser.add_argument('-v', '--xplane-version', type=int, default=10,
                        choices=[8, 9, 10],
                        help='Minimum X-Plane version to support.')
    extract_group = parser.add_mutually_exclusive_group()
    extract_group.add_argument('-x', '--extract-only', action='store_true',
                               help=('Extract all data found instead of '
                                     'converting it to X-Plane format.'))
    extract_group.add_argument('-l', '--library-path',
                               help=('Path to additional MS Flight Simulator '
                                     'library.'))
    debug_group = parser.add_mutually_exclusive_group()
    debug_group.add_argument('-p', '--profile', action='store_true',
                             help=('Enable profiler (for FS2XPlane '
                                   'developers).'))
    debug_group.add_argument('-d', '--debug', action='store_true',
                             help='Enable debugging.')
    return parser.parse_args()


def main():
    args = parse_args()
    logfile = abspath(join(args.xplane_scenery_path, 'summary.txt'))
    log = Log(logfile, args.debug)
    try:
        output = Output(args.msfs_scenery_path, args.library_path,
                        args.xplane_scenery_path, args.extract_only,
                        args.season, args.xplane_version, status, log,
                        refresh)
        output.scanlibs()
        if False:
            # Just list library uid/names.
            for (uid, (mdlformat, bglname, bglname, off, rcsize, name,
                       scale)) in output.libobj.iteritems():
                bgl = bglname[len(args.library_path):]
                log.debug("%s\t%s\t%s\n" % (uid, name, bgl))
            raise IOError
        if args.profile:
            from profile import run
            run('output.process()', join(args.xplane_scenery_path,
                                         'profile.dmp'))
        else:
            output.process()
        output.proclibs()
        output.procphotos()
        output.export()
        if exists(log.path) and not log.debug_enabled:
            status(-1, 'Displaying log "%s"' % log.path)
            log.view()
        status(-1, 'Done.')

    except FS2XError, e:
        if __debug__:
            print_exc()
        exit('Error:\t%s\n' % e.msg)

    except:
        status(-1, 'Internal error')
        print_exc()
        if not args.debug:
            log.info('\nInternal error\n')
            print_exc(None, log.file)
            status(-1, 'Displaying error log "%s"' % log.path)
            log.view()
        else:
            print
        exit(1)

    finally:
        log.close()


if __name__ == '__main__':
    # Path validation.
    mypath = dirname(abspath(argv[0]))
    if not isdir(mypath):
        exit('"%s" is not a folder' % mypath)
    if basename(mypath) == 'MacOS':
        # Starts in MacOS folder.
        chdir(normpath(join(mypath, pardir)))
    else:
        chdir(mypath)

    main()
