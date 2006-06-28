#!/bin/env python
#
# Simple setup.py to build the Zebra Python module (zebra.so)
#
from distutils.core import setup, Extension
import os
import re

#
# Build the list of extra link arguments from the EXTRA_LINK_ARGS environment
# variable, stripping leading & trailing whitespace and splitting on embedded
# whitespace.
#
whitespace = re.compile('\s+')
extra_link_args = re.sub(whitespace, ' ', os.getenv('EXTRA_LINK_ARGS').strip())
extra_link_args = extra_link_args.split(" ")

setup(name='zebra',
    ext_modules=[Extension('zebra', sources=['zebra.cc', 'msg.c', 'ds.c'],
                           extra_link_args=extra_link_args)])

