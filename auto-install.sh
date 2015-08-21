#!/bin/bash
# Install programs necessary for getting emacs working

status="summary"

# === Coding ===


# == Google Style Guide -> cpplint.py ==
if [[ -x /usr/local/bin/cpplint.py ]]
then
    msg="\e[32mGoogle cpplint.py already installed\e[39m"
else
    echo "== Installing Google cpplint.py =="
    # Download and move python script to proper location
    wget http://google-styleguide.googlecode.com/svn/trunk/cpplint/cpplint.py
    sudo mv cpplint.py /usr/local/bin/cpplint.py
    sudo chmod 755 /usr/local/bin/cpplint.py

    # Determine sucess
    if ! [[ -x /usr/local/bin/cpplint.py ]] ; then
       msg="\e[31mERROR: Google cpplint.py could not be installed\e[39m"
    else
       msg="\e[32mGoogle cpplint.py installed sucessfully\e[39m"
    fi    
fi
echo -e $msg && status="$status\n ${msg}"  # append message


echo -e "\n===\n$status\n===\n"
