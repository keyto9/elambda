# -*- coding: utf-8 -*-
import sys
import urllib.parse
import urllib.request

lmdsrc = sys.argv[1]
lmdsrc = open(lmdsrc, 'r').read()
params = {'lambda':lmdsrc}
ecdprm = urllib.parse.urlencode(params)
url = 'http://45.32.54.118:10086/verify_fibonacci?%s'
try:
	ret = urllib.request.urlopen(url % ecdprm).read()
except:
	ret = 'inet error'
print(ret)

