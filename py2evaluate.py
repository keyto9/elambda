# -*- coding: utf-8 -*-
import sys
import urllib

lmdsrc = sys.argv[1]
lmdsrc = open(lmdsrc, 'r').read()
params = {'lambda':lmdsrc}
ecdprm = urllib.urlencode(params)
url = 'http://45.32.54.118:10086/evaluate?%s'
try:
	ret = urllib.urlopen(url % ecdprm).read()
except:
	ret = 'inet error'
print(ret)

