set SBT_OPTS=-Xmx1024M -Dfile.encoding=UTF-8 -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M

java %SBT_OPTS% -jar sbt-launch.jar %*
