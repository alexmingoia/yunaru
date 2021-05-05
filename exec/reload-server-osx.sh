#!/usr/bin/env bash

# This script is used to automatically restart server and sign the executable
# to avoid OS X's network firewall dialogs.

pkill -9 tomo-server

codesign -s "My Development Signing Identity" -f $(stack exec -- which tomo-server)
DEBUG=1 stack exec tomo-server &
osascript -e 'display notification "Server restarted" with title "Development"'
