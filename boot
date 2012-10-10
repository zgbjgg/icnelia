#!/bin/sh
#
# ICNELIA : Helper for compile, clean and create run scripts for 
#           an erlang OTP application
#
# Copyright (C) 2012, Jorge Garrido <jorge.garrido@morelosoft.com>
#
# All rights reserved.
#

# creates dir ebin if not exists
echo "creating dirs ..." && mkdir -p ebin || { echo "error"; exit 1; }

# compiles the src code (erlang)
echo "compiling ..." && erlc -o ebin src/*.erl || { echo "error"; exit 1; }

# creates script executable from beam
# copy into script
echo "copying the files ..." && cat ebin/*.beam >> icnelia || { echo "error"; exit 1; }

# set headers for escript, make runnable on any machine
echo "building script ..." && sed -i '1i%%! -pa '$PWD'/ebin'  icnelia &&  sed -i '1i#!/usr/bin/env escript' icnelia || { echo "error"; exit 1; }

# set permissions for escript
echo "setting permissions ..." && chmod u+x icnelia || { echo "error"; exit 1; }

# send a help message 
echo "Icnelia has been compiled, now you can use the icnelia script to"
echo "	compile, clean and create run scripts for your erlang otp application"
