#!/usr/bin/env python
#$Id: PlatformDirWatcher.py,v 1.1 2004-12-02 20:58:27 burghart Exp $
import os, time, sys
from twisted.internet import reactor

"""
Platform watcher class that watches a Zebra platform data directory
for new files, and notifies the datastore to scan them.
"""

class FAM_Reader:
    _shared_state={}
    """Implement a singleton FAM connection"""
    def __init__(self, verbose = 0):
        # use the Borg "un-pattern", p208 of the "Python Cookbook" - since
        # we only want/need a single connection to FAM
        self.__dict__ = self._shared_state
        import _fam
        if not self.__dict__.has_key("_fam_connection"):
            self._fam_connection = _fam.open()
            self._fam_requests = {}
            self._userData = {}

    def logPrefix(self):
        "required method"
        return "FAM_Reader"
    
    def _process_fam_events(self, fe):
        if fe.userData:     
            fe.userData.handleEvent(fe)
        else :
            print 'Event w/o user data'
            print fe.filename, fe.code2str()

    def doRead(self):
        "called when fam connection has data to read"
        while self._fam_connection.pending():
            try :
                fe = self._fam_connection.nextEvent()
                self._process_fam_events(fe)       
            except IOError, er:
                # sometimes we get "unable to get next event"
                print "FAM_Reader.doRead():", er
                # close and re-open the FAM connection
                print "FAM_Reader.doRead() closing FAM connection"
                self._fam_connection.close()
                import _fam
                print "FAM_Reader.doRead() opening new FAM connection"
                self._fam_connection = _fam.open()

                for dir in self._fam_requests.keys():
                    self.monitorDirectory(dir, self._userData[dir])
                return

    def monitorDirectory(self, dir, userData = None):
        self._userData[dir] = userData
        self._fam_requests[dir] = \
            self._fam_connection.monitorDirectory(dir, userData)

    def fileno(self):
        "return the file number of this FAM connection"
        return self._fam_connection.fileno()

    def connection(self):
        "return the fam connection"
        return self._fam_connection
    
    def connectionLost(self, args):
        "required method"
        print 'lost FAM connection'

    def closeConnection(self):
        self._fam_connection.close()

class PlatformDirWatcher:
    """Monitor a directory for new files"""

    def __init__(self, platformName, dir, zebraBinDir, verbose = 0):
        """
        platname - Zebra platform name
        dir - which directory to watch
        only_new - only notify for  newly created files
        zebraBinDir - where Zebra binaries are found, defaults to the directory
                      where this script lives
        """
        # register the FAM client
        fam_rdr = FAM_Reader()
        reactor.addReader(fam_rdr)
        self._platformName = platformName
        self._dir = dir
        fam_rdr.monitorDirectory(self._dir, self)

        self._zebraBinDir = zebraBinDir
        self._dsRescan = os.path.join(self._zebraBinDir, 'dsrescan')
        
        self._verbose = verbose

    def handleEvent(self, event):
        # we don't care about source directory or subdirectories,
        # only about src directory contents
        fullname = os.path.join(self._dir, event.filename)
        if os.path.isdir(fullname):
            return

        
        root, ext = os.path.splitext(event.filename)
        if (ext == ".part" or ext == ".tmp"):
            if self._verbose:
                print 'Ignoring temporary file', event.filename
            return
        
        eventCode = event.code2str()
        if eventCode == 'created':
            if self._verbose: print 'Adding %s to datastore' % fullname
            self._processFile(event.filename)
        elif eventCode == 'exists':
            if self._verbose: print 'Ignoring existing file', fullname
            return
#        else:
#            print "handleEvent: ", event.filename, event.code2str()

    def _processFile(self, fileName):
        cmd = "%s -file %s %s" % (self._dsRescan, fileName,
                                  self._platformName)
        if self._verbose:
            print "executing:", cmd
        status = os.system(cmd)
        if status:
            print "ERROR", status, "from PlatformDirWatcher command:", cmd

if __name__ == '__main__':
    argc = len(sys.argv)
    if argc < 3 or argc > 4:
        print("Usage: %s <platform> <platform_dir> [<zebra_bin_dir>]" %
              sys.argv[0])
        sys.exit(1)
    platform = sys.argv[1]
    platDir = sys.argv[2]
    #
    # If we're not given a Zebra binary dir, use the directory
    # where this script lives
    #
    if (argc == 4):
        zebraBinDir = sys.argv[3]
    else:
        zebraBinDir, file = os.path.split(sys.argv[0])

    dir_watcher = PlatformDirWatcher(platform, platDir, zebraBinDir,
                                     verbose = 0)
    reactor.run()
