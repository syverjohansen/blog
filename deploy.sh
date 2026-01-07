#!/bin/bash

echo -e "\033[0;32mDeploying updates to GitHub...\033[0m"

# First, commit and push changes in ~/ski directory
echo -e "\033[0;33mUpdating ski directory...\033[0m"
cd ~/ski
git add .

# Commit changes in ski directory
ski_msg="Updated ski data and models `date`"
if [ $# -eq 1 ]
  then ski_msg="$1 - ski updates"
fi
git commit -m "$ski_msg"

# Pull and push ski directory
git pull origin main
git push origin main

# Now handle blog directory
echo -e "\033[0;33mUpdating blog directory...\033[0m"
cd ~/blog/daehl-e

# Add changes to blog git
git add .

# Commit changes in blog directory
blog_msg="Updated blog content `date`"
if [ $# -eq 1 ]
  then blog_msg="$1 - blog updates"
fi
git commit -m "$blog_msg"

# Pull and push blog directory
git pull origin main
git push origin main

echo -e "\033[0;32mAll repositories updated successfully!\033[0m"
