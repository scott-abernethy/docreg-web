#!/bin/bash
if [ -z "$JREBEL_HOME" ]; then
  # Start as per normal...
  java -XX:MaxPermSize=1024m -Xmx512M -Xss2M -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launcher.jar "$@"
else
  # Start with JRebel agent...
  java -XX:MaxPermSize=1024m -Xmx512M -Xss2M -XX:+CMSClassUnloadingEnabled -javaagent:$JREBEL_HOME/jrebel.jar -jar `dirname $0`/sbt-launcher.jar "$@"
fi
