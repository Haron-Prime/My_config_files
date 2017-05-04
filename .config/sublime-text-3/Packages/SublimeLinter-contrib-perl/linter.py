#
# linter.py
# Linter for SublimeLinter3, a code checking framework for Sublime Text 3
#
# Written by Gregory Oschwald
# Copyright (c) 2013 Gregory Oschwald
#
# License: MIT
#

"""This module exports the Perl plugin class."""

import shlex
from SublimeLinter.lint import Linter, util


class Perl(Linter):

    """Provides an interface to perl -c."""

    syntax = ('modernperl', 'perl')
    executable = 'perl'

    regex = r'(?P<message>.+?) at - line (?P<line>\d+)(, near "(?P<near>.+?)")?'
    error_stream = util.STREAM_STDERR

    def cmd(self):
        """
        Return the command line to execute.

        Overridden so we can add include paths based on the 'include_dirs'
        settings.

        """

        command = [self.executable_path, '-c']

        include_dirs = self.get_view_settings().get('include_dirs', [])

        for e in include_dirs:
            command.append('-I')
            command.append(shlex.quote(e))

        return command
