# Workaround way to launch Google Chrome while setting the URI directly
# this supports custom URIs like "slack://channel
# Otherwise OSX "open" command will try to interpret URI as http/https

URI=$1

osascript <<EOD
set theURL to "$URI"
tell application "Google Chrome"
 if windows = {} then
  make new window
  set URL of (active tab of window 1) to theURL
 else
  make new tab at the end of window 1 with properties {URL:theURL}
 end if
 activate
end tell
EOD
