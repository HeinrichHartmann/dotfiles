#!/usr/bin/python

from datetime import datetime

GAME_END = datetime(2014,07,13,00,00,00)

WORLD_START = datetime(1000,1,1,0,0)

nnow = WORLD_START + (datetime.now() - GAME_END)

print "{:04d}-{:02d}-{:02d} {:02d}:{:02d}:{:02d}".format(nnow.year - 1000, nnow.month, nnow.day, nnow.hour, nnow.minute, nnow.second)
