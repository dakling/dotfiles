#!/usr/bin/env sh

# synclient TapButton1=0
# synclient TapButton2=0
# synclient TapButton3=0

# synclient TapButton1=1
# synclient TapButton2=3
# synclient TapButton3=2

synclient TapButton1=$(synclient | awk '/TapButton1/ { off=int($3) }
                                         END         { print !off }')
