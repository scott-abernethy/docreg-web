#!/bin/bash
if [ -z "$JREBEL_HOME" ]; then
  JREBEL_HOME=/opt/JRebel
fi
java -XX:MaxPermSize=786m -Xmx712M -Xss2M -XX:+CMSClassUnloadingEnabled -noverify -javaagent:$JREBEL_HOME/jrebel.jar -jar `dirname $0`/sbt-launcher.jar "$@"
