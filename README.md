flicker
=======

Simple tool to display flicker codes for the german chipTAN / Sm@rtTAN
online banking tan systems.

Installation
============

1. Install [stack](https://haskellstack.org)
2. `git clone https://github.com/talanis85/flicker.git`
3. `cd flicker && stack install`

Usage
=====

  `flicker <mime-type> <flickercode>`

`<mime-type>` is usually `text/x-flickercode` but it can be anything
since we just ignore it. The parameter is there for aqbanking compatibility.

`<flickercode>` is a hex-encoded string of half-bytes representing the
challenge to be solved.

Usage with aqbanking-cli
========================

Add `--opticaltan=/path/to/flicker` to your aqbanking-cli calls.

Keyboard commands
=================

* `Up Arrow`: Increase Size
* `Down Arrow`: Decrease Size
* `Left Arrow`: Slower
* `Right Arrow`: Faster
