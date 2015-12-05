#!/usr/bin/python


import sys, random
args = sys.argv

N = -1
if "-n" in args:
    i = args.index('-n')
    args.pop(i)
    N = int(args.pop(i))

fn = args.pop()

with open(fn) as fh:
    lines=fh.readlines()

c = 0
while c != N:
    sys.stdout.write(random.choice(lines))
    c += 1
