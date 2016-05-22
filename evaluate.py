# -*- coding: utf-8 -*-
import sys
import urllib.parse
import urllib.request

lmdsrc = sys.argv[1]
lmdsrc = open(lmdsrc, 'r').read()
params = {'lambda':lmdsrc}
ecdprm = urllib.parse.urlencode(params)
url = 'http://localhost:10086/evaluate?%s'
try:
	ret = urllib.request.urlopen(url % ecdprm).read()
except:
	ret = 'inet error'
print(ret)

