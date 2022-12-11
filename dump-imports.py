#!/usr/bin/env python
import sys
import os
from glob import glob
from subprocess import Popen, PIPE

fname = sys.argv[1]

proc = Popen(['ghci', '-ddump-minimal-imports', fname], stdin=PIPE)
proc.stdin.close()
proc.wait()

base, _ = os.path.splitext(os.path.basename(fname))

files = glob('*.imports')
importname = f'{base}.imports' if f'{base}.imports' in files else 'Main.imports'

with open(importname, 'r') as fp:
    print(fp.read())

[os.remove(imp) for imp in files]
