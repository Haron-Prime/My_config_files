#
# linter.py
# Linter for SublimeLinter3, a code checking framework for Sublime Text 3
#
# Written by Patrick Kish
# Copyright (c) 2014 Patrick Kish
#
# License: MIT
#
"""This module exports the LuaGlobals plugin class."""

from os.path import dirname, join, realpath
FOLDER_PATH = dirname(realpath(__file__))

from SublimeLinter.lint import Linter


class LuaGlobals(Linter):

    """Provides an interface to lua-globals."""

    syntax = 'lua'
    script_path = join(FOLDER_PATH, 'findglobals.lua')
    cmd = 'lua "' + script_path + '" "@"'
    regex = (
        r'\s*\[(?P<line>\d+)\]\s+'
        r'((?P<warning>G:)|(?P<error>S:))'
        r'(?P<message>.+?(?::\s(?P<near>.*)|$))'
    )
    tempfile_suffix = "lua"
