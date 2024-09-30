# evkoverlay
evkoverlay is a keyboard overlay utility that runs on Linux and
particularly Wayland because it grabs input directly from Linux's evdev
interface. This unfortunately means that it has to be run as root in
order to be able to access files in /dev/input/, but I really cannot
think of any way around that for now.

## Building
This tool is written in Haskell and is built using
[stack](https://docs.haskellstack.org/en/stable/), just run `stack
install` to build and install the executable.

## Dependencies
[Raylib](https://www.raylib.com/) is needed.

## Usage
Before usage, you _must_ change the value of `configInputFile` in
`app/Main.hs` (and of course rebuild) to whatever your keyboard
device is. To find out, look in `/proc/input/by-path` and look for
any file that vaguely looks like a keyboard (kbd, keyboard, etc.),
for example `/dev/input/by-path/platform-i8042-serio-0-event-kbd`.
This only needs to be done once.

As mentioned, this tool must be run with elevated privileges (`sudo
evkoverlay`) in order to access raw input devices. Configuration,
e.g. changing your keys or layout, can be done by editing
`app/Main.hs` and rebuilding. The values of `configSizeofTimeT = 8` and
`configSizeofSusecondsT = 8` should be correct for most people, but if
you are on an exceptionally odd system they may require changing.
