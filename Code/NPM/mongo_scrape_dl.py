# scrapes MongoDB to collect data for NPM package downloads
# Usage: python3 <script> <output_filename>(optional) [<dbname>(optional) <collname>(optional)]

import pymongo, json, sys

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


#For creating Header
i = 0
j = 0
if (len (sys .argv) > 1):
    fname = str(sys .argv [1])
else:
    fname = 'NPM_dl.csv'
    
f = open(fname,'w')

if (len (sys .argv) > 3):
    dbname = str(sys .argv [2])
    collname = str(sys .argv [3])
else:
    dbname = 'NPM_Popular_Package_Download'
    collname = 'daily_downloads_popular'
db = client [dbname]
coll = db[collname]
l = coll.count()
print(l)

    
for r in coll.find():
    r.pop('_id', None)
    j += 1
    printProgressBar(j, l, prefix = 'Progress:', suffix = 'Complete', length = 50, decimals = 2)
    if i == 0: 
        data = 'Package.Name,'
        v = list(r.values())[0]
        #print(type(v))
        for e in v:
            data = data + str(e['day'])+','
        data = data[:-1]+'\n'
        i = 1
        ldt = len(v)
        f.write(data)
    x = list(r.keys())
    #print(x[0], ldt)

    data = str(x[0])+','
    v = list(r.values())[0]
    if len(v) != ldt: 
        #print(x[0])
        continue
    for e in v:
        data = data + str(e['downloads'])+','
    data = data[:-1]+'\n'
    if len(data.split(',')) == ldt +1:
        f.write(data)
    else:
        print(x[0], len(data.split(',')), ldt)
        continue
    
f.close()        
