import sys, re, pymongo, json, time
import datetime
from requests.auth import HTTPBasicAuth
import requests

# python3 <script> <collection parameter> <GitHub user name> <password> < <input json file - {key = repo: value = list ofpackage names}>

def printProgressBar (iteration, total, prefix = '', suffix = '', decimals = 1, length = 100, fill = '█'):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
    """
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filledLength = int(length * iteration // total)
    bar = fill * filledLength + '-' * (length - filledLength)
    print('\r%s |%s| %s%% %s' % (prefix, bar, percent, suffix), end = '\r')
    # Print New Line on Complete
    if iteration == total: 
        print()

###################################################

login = ""
passwd = ""
###################################################

client = pymongo.MongoClient (host="da1.eecs.utk.edu")
db = client ['NPM_Popular_Package_Download']
#popular pkgs
coll = db['Issues']


baseurl = 'https://api.github.com/repos'
headers = {'Accept': 'application/vnd.github.v3.star+json'}
headers = {'Accept': 'application/vnd.github.v3+json'}
#do for followers following starred subscriptions orgs gists repos events received_events 
collName = 'issues?state=all'
if (len (sys .argv) > 1):
  collName = sys .argv [1]
if (len (sys .argv) > 2):
  login = sys .argv [2]
if (len (sys .argv) > 3):
  passwd = sys .argv [3]

# Reference a particular collection in the database
#coll = db [collName]

gleft = 0

def wait (left):
  while (left < 20):
    l = requests .get('https://api.github.com/rate_limit', auth=(login,passwd))
    if (l.ok):
      left = int (l.headers.get ('X-RateLimit-Remaining'))
      reset = int (l.headers.get ('x-ratelimit-reset'))
      now = int (time.time ())
      dif = reset - now
      if (dif > 0 and left < 20):
        sys.stderr.write ("waiting for " + str (dif) + "s until"+str(left)+"s\n")
        time .sleep (dif)
    time .sleep (0.5)
  return left  

def get (url):
  global gleft
  gleft = wait (gleft)
  values = []
  size = 0
  # sys.stderr.write ("left:"+ str(left)+"s\n")
  try: 
    r = requests .get (url, headers=headers, auth=(login, passwd))
    time .sleep (0.5)
    if (r.ok):
      gleft = int(r.headers.get ('X-RateLimit-Remaining'))
      lll = r.headers.get ('Link')
      links = ['']
      if lll is not None: 
        links = lll.split(',')
      t = r.text
      size += len (t)
      try:
        array = json .loads (t)
        for el in array:
          values .append (el)
      except Exception as e:
        sys.stderr.write(str(e)+" in json .loads\n")
      #t = r.text.encode ('utf-8')
      
      if len(links) > 1: 
       while '; rel="next"' in  links[0] or '; rel="next"' in  links[1]:
        
        gleft = int(r.headers.get ('X-RateLimit-Remaining'))
        gleft = wait (gleft)
        if '; rel="next"' in  links[0]:
            url = links[0] .split(';')[0].replace('<','').replace('>','').strip();
        else:
            url = links[1] .split(';')[0].replace('<','').replace('>','').strip();
        
        try: 
          r = requests .get(url, headers=headers, auth=(login, passwd))
          if (r.ok): 
            lll = r.headers.get ('Link')
            links = ['']
            if lll is not None: 
              links = lll .split(',')
            t = r.text
            size += len (t)
            tc = 0
            try:
              array = json.loads (t)
              for el in array:
                values .append (el)
              
            except Exception as e:
              sys.stderr.write(str(e)+" in json .loads next\n")
          else:
            # Adding Retry code
            time.sleep(0.5)
            try: tc += 1
            except: tc = 0
            if tc > 5:
                sys.stderr.write (url + ';ERROR r not ok\n')
                tc = 0
          
        except requests.exceptions.ConnectionError:
          sys.stderr.write('could not get ' + links + ' for '+ url + '\n')   
        
    else:
      sys.stderr.write (url + ';ERROR r not ok\n')
  except requests.exceptions.ConnectionError:
    sys.stderr.write (url + ';ERROR ConnectionError')
  return values, size


# Read in data
data = json.load(sys.stdin)
l = len(data.keys())
print(l)
j = 0
for key in data.keys():
    j += 1
    n = key.strip ()
    
    if '.js' in n:
        r = requests.get('https://github.com/' + n)
        if r.ok: 
            pass
        else:
            r = requests.get('https://github.com/' + n.replace('.js',''))
            if r.ok:
                n =  n.replace('.js','')
            else:
                sys.stderr.write (n + ';Repo Not Found\n')
    #else:
        #continue
    
    url1 = baseurl + '/' + n + '/' + collName
    v = []
    size = 0
    try: 
        v, size = get (url1)
        #break
        #print (str (len (v)) + ';' + str (size) + ';' + url1)
        #sys .stdout .flush ()
    except Exception as e:
        sys.stderr.write ("Could not get:" + url1 + ". Exception:" + str(e) + "\n")
        continue
    #print (url1 + ' after exception lenv(v)=' + str(len (v)))
    #print(url1)
    #break
    i = 0
    if v == []:
        continue
    for e in v:
        entry = {'packages': data[key], 'issue': e, 'number': i }
        i += 1
        coll .insert (entry, check_keys=False)
    printProgressBar(j, l, prefix = 'Progress:', suffix = 'Complete', length = 50, decimals = 2)
    