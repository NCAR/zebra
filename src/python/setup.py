#!/bin/env python
#
# Simple setup.py to build the Zebra Python module (zebra.so)
#
from distutils.core import setup, Extension

setup(name='zebra',
      ext_modules=[Extension('zebra', sources=['zebra.c', 'msg.c', 'ds.c'])])

