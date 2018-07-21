Task
====

Investigate the relationship between the deployment
(number of new visits and number of new users (visits))
and numbers of exceptions and fatal exceptions
(crashes).



Data Description
================

Each file (e.g., UA-36442576-1.new) corresponds to output from one
app. The long number is the account, the specific slice for that app
such as UA-36442576-1 AC-Android Pre-GA, indicates that data was
collected only for the pre-GA releases.

Please select several more popular apps:
```
UA-36442576-4.new
UA-40745335-3.new
UA-44698255-1.new
UA-56161929-1.new
```


```
R --no-save
> for (ll in system("ls *.new",intern=T)){
+   x = read.csv(ll,sep=';');
+   print(paste(ll, " ", sum(x$ga.visits)))
+ }
[1] "UA-36442576-1.new   139075"
[1] "UA-36442576-3.new   160"
[1] "UA-36442576-4.new   6238662"
[1] "UA-36442576-5.new   773585"
[1] "UA-40745335-1.new   878247"
[1] "UA-40745335-2.new   41647"
[1] "UA-40745335-3.new   14584603"
[1] "UA-44698255-1.new   6836351"
[1] "UA-56161929-1.new   6283053"
[1] "UA-56161929-2.new   19"
```

Each table has following fields (collected each day)
```
ga:appVersion - version of the app (to match releases)
ga:operatingSystemVersion  - version of Android (or iOS)
ga:date  
ga:exceptions   - bad events
ga:fatalExceptions - worst events
ga:newVisits   - new users using app
ga:visits;     - total usages of the app
ga:timeOnSite  - amount of time spent using app
```


There is mobileRel.csv that gives official release dates, but its
better to estimate them from data (when a certain version appears).




```
36442576 AC-Android
  UA-36442576-1 AC-Android Pre-GA
    66275034 AC-Android Pre-GA All Mobile App Data
  UA-36442576-3 AC-Android Experimental
    72444718 AC-Android Experimental All Mobile App Data
  UA-36442576-4 AC-Android GA
    79349574 AC-Android GA All Mobile App Data
  UA-36442576-5 AC-Android Dev
    79912722 AC-Android Dev All Mobile App Data
    99871005 AC-Android 2.1.1_566 Mobile App Data (begins Mar 24)
40745335 1xmsipios
  UA-40745335-1 1XM SIP iOS
    72167750 All Mobile App Data
  UA-40745335-2 1XM SIP iOS Beta
    79648078 1XM SIP iOS Beta All Mobile App Data
  UA-40745335-3 1XM SIP iOS GA
s    80694328 1XM SIP iOS GA All Mobile App Data
44698255 Avaya Mobile Apps
  UA-44698255-1 ScsCommander
    77543105 All Mobile App Data
56161929 AC-iPhone
  UA-56161929-1 Avaya Communicator for iPhone
    93054549 All Mobile App Data
    104869929 Only App Version 2.1 Data
  UA-56161929-2 Communicator analytics test
    112953277 All Mobile App Data
```

Customer found defects:
grep -E '^(FI|FA|NGUE|MSI|ONEXMOBILE|ONEXSIPIOS|SCAE|WEBRTCENGINE)-' \
  IPOJIRACFDs.csv > mobile.csv

Release dates for various mobile apps:
grep -EiA10 'ios|android|flare|communicat' programCenter.csv > mobileRel.csv

