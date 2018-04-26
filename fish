#!/bin/sh

if ( ! which java >&/dev/null ); then
    echo -e "\nJava could not be found. Please ensure you have Java version 5 or later\ninstalled and that the 'java' command is on your path.\n"
    exit 1;
fi

if [[ $1 = -c* ]]; then
    java -cp bin:lib/jcommon-1.0.23/jcommon-1.0.23.jar:lib/jfreechart-1.0.19/lib/jfreechart-1.0.19.jar -Djava.library.path=bin fish.Fish
else
    java -cp bin:lib/jcommon-1.0.23/jcommon-1.0.23.jar:lib/jfreechart-1.0.19/lib/jfreechart-1.0.19.jar -Djava.library.path=bin fish.Fish >&/dev/null
fi
