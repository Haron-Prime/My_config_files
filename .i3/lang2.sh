#!/bin/sh
LOC=$(xset -q|grep LED| awk '{ if (substr ($10,5,1) == 1) print "RU"; }') 
if [[ "$LOC" == "RU" ]]; then 
	TEXT='<span foreground="#ffbf00">RU</span>';
else 
	TEXT='<span foreground="#aaddff">EN</span>';
fi
echo $TEXT
exit 0
