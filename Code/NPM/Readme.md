# Analysis of NPM issues

process.ipynb - used to get the list of packages with more than 10k downloads.

npm_dl_pkg.py - used to get download statistics for NPM packages and save in mongoDB

mongo_scrape_dl.py - Used to scrape MongoDB for download data and create csv file

Git repo NPM.ipynb - used to get GitHub repos of NPM packages

ghCollab.py - used to get all issues for packages from GitHub and save them in MongoDB

mongo_scrape_issues.py - used to scrape MongoDB for issue data

process_issue.py - further process issue data for analysis

issue_timeline.ipynb - final analysis of issues
