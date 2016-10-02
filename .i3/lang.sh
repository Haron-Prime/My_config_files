#/bin/sh
LOC=$(skb 1)
if [ "$LOC" == "Eng" ]; then
		TEXT='<span foreground="#aaddff">EN</span>';
	else 
		TEXT='<span foreground="#ffbf00">RU</span>';
fi
echo $TEXT
exit 0