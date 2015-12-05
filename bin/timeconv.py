#!/usr/bin/python

import sys
import re

d,h,m,s = re.match("(\d*d)?(\d*h)?(\d*m)?(\d*s?)?",sys.argv[1]).groups()

if not d: d = "0d"
if not h: h = "0h"
if not m: m = "0m"
if not s: s = "0s"

d = d.strip("d")
h = h.strip("h")
m = m.strip("m")
s = s.strip("s")

print int(s) + 60*(int(m) + 60*(int(h) + 24 * int(d)))
