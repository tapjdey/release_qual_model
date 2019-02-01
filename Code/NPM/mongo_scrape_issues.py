# scrapes MongoDB to collect data for NPM package issues
# saves issue creation dates with package name in json
# Usage: python3 <script> <outfile>(optional)

import pymongo, json, sys, csv

#progress bar
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


client = pymongo.MongoClient(host="da1.eecs.utk.edu")

db = client ['NPM_Popular_Package_Download']
coll = db['Issues']
l = coll.count()

if (len (sys .argv) > 1):
    fname = str(sys .argv [1])
else:
    fname = 'issues_10k.json'
    
j = 0
issue_count_d = {}

for r in coll.find():
    r.pop('_id', None)
    j += 1
    printProgressBar(j, l, prefix = 'Progress:', suffix = 'Complete', length = 50, decimals = 2)
    for pkgname in r['packages']:
        if pkgname in issue_count_d.keys():
            try:
                issue_count_d[pkgname].append(r['issue']['created_at'])
            except:
                pass
        else:
            try:
                issue_count_d[pkgname] = [r['issue']['created_at']]
            except:
                issue_count_d[pkgname] = []

#outstr = 'Package, Issues'
#for key in issue_count_d.keys():
#    outstr += ('\n'+ key +',' + str(issue_count_d[key]))
        
#with open('issue_count.csv', 'w') as f:  
#    f.write(outstr)

with open(fname, 'w') as outfile:
    json.dump(issue_count_d, outfile)
     
