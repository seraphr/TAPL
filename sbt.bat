set SBT_OPTS=-Xmx1024M -Dinput.encoding=Cp1252 -Dfile.encoding=SJIS -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M

java %SBT_OPTS% -jar sbt-launch.jar %*
