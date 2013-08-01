set SCRIPT_DIR=%~dp0
java -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=512m -Xmx1024M -Xss2M -jar "%SCRIPT_DIR%\sbt-launcher.jar" %*