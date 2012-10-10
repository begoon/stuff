#!/usr/bin/python

import sys, re

data = sys.stdin.read()

data = data[data.index("<body>") + 6:]
data = data[0:data.index("</body>")]

p = re.compile('^\s*<div class="document">\s*', re.DOTALL)
data = p.sub('', data)

p = re.compile('\s*</div>\s*$', re.DOTALL)
data = p.sub('', data)

p = re.compile('class="reference external" ', re.DOTALL)
data = p.sub('', data)

p = re.compile('<img alt="[^"]+" src="([^"]+)" />', re.DOTALL)
data = p.sub(r'<a rel="lytebox" href="@\1@"><img src="\1" /></a>', data)

p = re.compile('@(.*?)_preview(.*?)@', re.DOTALL)
data = p.sub(r'\1\2', data)

print data
