#!/bin/bash


psc `find bower_components -name \*.purs | grep -v test | grep -v example` src/Main.purs --main=Main --module=Main > bundle.js
