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
As mentioned, this tool must be run with elevated privileges (`sudo
evkoverlay`) in order to access raw input devices. Configuration,
e.g. changing your keys or layout, can be done by editing
`app/Main.hs` and rebuilding. The values of `configSizeofTimeT = 8` and
`configSizeofSusecondsT = 8` should be correct for most people, but if
you are on an exceptionally odd system they may require changing.
