#!/usr/bin/env python
from gtk import *
import GtkExtra,sys,os
def delete_event(win, event=None):
    win.hide()
    # don't destroy window -- just leave it hidden
    return TRUE



def create_file_selection(_button):
    win = GtkFileSelection("file selection dialog")
    def file_selection_ok(_button, fs=win):
	filename = fs.get_filename()
	print filename
	os.system("ac_convert.py %s | actrack -input " % filename) 
	fs.hide()
    win.connect("delete_event", delete_event)
    win.ok_button.connect("clicked", file_selection_ok)
    win.cancel_button.connect("clicked", win.hide)
    win.show()
    return win

def do_exit(_button):
    sys.exit(0)

def create_main_window():
    win = GtkWindow()
    win.set_name("Aircraft Tracks batch processing")
    box1 = GtkVBox(FALSE, 0)
    win.add(box1)
    box1.show()
    button = GtkButton("Select Aircraft Tracks File")
    button.connect("clicked", create_file_selection)
    button.show()
    box1.pack_start(button)

    quitb = GtkButton("Exit")
    quitb.connect("clicked", do_exit)
    quitb.show()
    box1.pack_start(quitb)

    win.connect("destroy", mainquit)
    win.connect("delete_event", mainquit)
    win.set_title("Aircraft Tracks batch processing")
#    fs = create_file_selection(button)
    win.show()

def main():
#    rc_parse("testgtkrc")
    create_main_window()
    mainloop()

if __name__ == '__main__': main()
 
