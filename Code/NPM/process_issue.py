# Create Issue timeline for NPM packages
# run: python3 <script> [json of issue creation dates for each package](optional) 
# change dates in the file

import json, sys
from datetime import datetime, timedelta

#progress bar
def printProgressBar (iteration, total, prefix = 'Progress', suffix = 'Complete', decimals = 1, length = 100, fill = '█'):
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

if (len (sys .argv) > 1):
    fname = sys .argv [1]
else:
    fname = 'issues_10k.json'

with open(fname, 'r') as infile:
    data = json.load(infile)

start_date = datetime(2015, 3, 1)
delta = timedelta(days=1)
end_date = datetime(2018, 8, 31)
i = 0
j = 0
lt = len(data.keys())

#Outfile
f = open('issue_10k_timeline.csv','w')


for key in data.keys():
    printProgressBar(j, lt)
    j += 1
    issue_dates = data[key]
    idt_t = [datetime.strptime(x, '%Y-%m-%dT%H:%M:%SZ') if datetime.strptime(x, '%Y-%m-%dT%H:%M:%SZ')> start_date\
                     else start_date for x in issue_dates]
    idt_f = [x.date() for x in idt_t if x<end_date]
    dt = start_date
    if i == 0:
        outstr = 'Package'
        while dt <= end_date:
            outstr += ','+str(dt.strftime('%Y-%m-%d'))
            dt += delta
        outstr += '\n'
        f.write(outstr)
        dt = start_date
        i = 1
    outstr = str(key)
    while dt <= end_date:
        dt += delta
        outstr += ','+str(sum(1 for x in idt_f if x<= dt.date()))
    outstr += '\n'
    f.write(outstr)
    
f.close()        
    