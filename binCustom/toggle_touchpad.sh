#!/usr/bin/env sh

synclient TouchpadOff=$(synclient | awk '/TouchpadOff/ { off=int($3) }
                                         END           { print !off }')
