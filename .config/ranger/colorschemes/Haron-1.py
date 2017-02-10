#!/usr/bin/python

# This file is custom clorscheme of ranger, the console file manager.
# License: WTFPL © 2017  http://www.wtfpl.net/
# Author: Haron Prime

from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import *


class Haron(ColorScheme):
    progress_bar_color = 30

    def use(self, context):
        fg, bg, attr = default_colors

        if context.reset:
            return default_colors

        elif context.in_browser:
            fg = 246
            if context.selected:
                attr = reverse
            else:
                attr = normal
            if context.empty or context.error:
                fg = 235
                bg = 45
            if context.border:
                fg = default
            if context.media:
                if context.image:
                    fg = 107
                else:
                    fg = 107
            if context.container:
                fg = 94
            if context.directory:
                fg = 24
            elif context.executable and not \
                    any((context.media, context.container,
                        context.fifo, context.socket)):
                fg = 71
                attr |= bold
            if context.socket:
                fg = 107
                bg = 230
                attr |= bold
            if context.fifo:
                fg = 107
                bg = 230
                attr |= bold
            if context.device:
                fg = 244
                bg = 230
                attr |= bold
            if context.link:
                fg = context.good and 30 or 160
                attr |= bold
                if context.bad:
                    bg = 235
            if context.tag_marker and not context.selected:
                attr |= bold
                if fg in (red, 141):
                    fg = white
                else:
                    fg = red
            if not context.selected and (context.cut or context.copied):
                fg = 234
                attr |= bold
            if context.main_column:
                if context.selected:
                    attr |= bold
                if context.marked:
                    attr |= bold
                    bg = 237
            if context.badinfo:
                if attr & reverse:
                    bg = 141
                else:
                    fg = 141

            if context.inactive_pane:
                fg = 241

        elif context.in_titlebar:
            attr |= bold
            if context.hostname:
                fg = context.bad and 16 or 45
                if context.bad:
                    bg = 166
            elif context.directory:
                fg = 30
            elif context.tab:
                fg = context.good and 47 or 30
                bg = 238
            elif context.link:
                fg = 38

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = 117
                elif context.bad:
                    fg = 160
                    bg = 235
            if context.marked:
                attr |= bold | reverse
                fg = 237
                bg = 47
            if context.message:
                if context.bad:
                    attr |= bold
                    fg = 160
                    bg = 235
            if context.loaded:
                bg = self.progress_bar_color

        if context.text:
            if context.highlight:
                attr |= reverse

        if context.in_taskview:
            if context.title:
                fg = 117

            if context.selected:
                attr |= reverse

            if context.loaded:
                if context.selected:
                    fg = self.progress_bar_color
                else:
                    bg = self.progress_bar_color

        return fg, bg, attr
