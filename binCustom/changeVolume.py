#!/usr/bin/python

import sys
import alsaaudio

arg = sys.argv[1]
volDiff = int(arg)

#get Information
mixer_name = alsaaudio.mixers()[0]
mixer = alsaaudio.Mixer(mixer_name)

newVol = max(mixer.getvolume()[0], mixer.getvolume()[1]) + volDiff
mixer.setvolume(newVol)

