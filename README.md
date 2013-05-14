Just a small learning experiment in Aeson and Gloss. 

Not an example of good haskell.

It can load a random palette list from a request to the API, or from some local json files.

Usage:
======
    prog

To run in fullscreen mode, add a -f flag:
    prog -f

Keys:
-----
 * Space              : load a new random palette from ColourLovers.com
 * 'l'                : load a new random palette list from file (limited)
 * 'r'                : rotate the palette stripes
 * 'f'                : favourite a palette (save to an in-memory list)
 * 'F'                : load palette list from your favourites (in-memory only)
 * Cursor Left, Right : Change palette within the current list
 * Cursor Up, Down    : Change the brightness of the palette (automatically, dumbly, two-toned)
 * 'i'                : toggle palette info
 * 'd'                : change dimming function set [(light, dark), (bright, dim)]
 * Enter              : Change anim speed

Screenshots:
============
![screenshot of first gui](https://github.com/nbogie/colorlovers-api-demo/raw/master/docs/screenshots/Screenshot.png)

![another screenshot of first gui](https://github.com/nbogie/colorlovers-api-demo/raw/master/docs/screenshots/Screenshot-two-tone.png)

