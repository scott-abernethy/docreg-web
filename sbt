#!/bin/bash
HOMEDIR=~
java -XX:MaxPermSize=786m -Xmx712M -Xss2M -XX:+CMSClassUnloadingEnabled -noverify -javaagent:$HOMEDIR/bin/jrebel/jrebel.jar -jar `dirname $0`/sbt-launcher.jar "$@"
