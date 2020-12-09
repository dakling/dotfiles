#!/usr/bin/env bash

function play-end-of-round-sound () {
    for (( n = 0; n < 2; n++ ))
    do
        mpv ~/Musik/A-Tone-His_Self-1266414414.mp3 >/dev/null 2>&1
    done
}

function play-end-of-exercise-sound () {
    mpv ~/Musik/Electronic_Chime-KevanGC-495939803.mp3 >/dev/null 2>&1
}

function exercise (){
    clear
    echo "Round number $j"
    for (( i = $2; i >= 0; i-- ))
    do
        tput cup $k $l
        # echo -n "$1 $i"
        printf "$1 %02d " $i
        sleep 1
    done
    play-end-of-exercise-sound
    k=$(($k+1))
    echo

}

function workout () {
    for (( j = 0; j < $1 ; j++ ))
    do
        k=1
        if (( j == 0 ))
        then {
            exercise "Get ready!" 10
        }
        fi
        exercise "Jog in place" 50
        exercise "Jumping Jacks" 20
        exercise "Jog in place" 30
        exercise "Sprint" 20
        exercise "Jog in place" 30
        exercise "Burpies" 30
        exercise "Drink / Active Rest" 10
        play-end-of-round-sound
    done
    echo "Woop woop! You are done with your workout for today!"
}

workout 10
