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

print data
