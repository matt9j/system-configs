#! /usr/bin/env bash

# Clear all the window size and location preferences from the snap
# spotify config cache. A cleaner version of this would only clear the
# latest config, but this calls for drastic measures! :o
find ~/snap/spotify -type f -name prefs \
     -exec echo "clearing " {} \;\
     -exec grep app.window.position {} \;\
     -exec sed -i '/app.window.position/d' {} \;
