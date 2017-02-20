# auto deploy server

# get last commit hash
[ -f last.hash ] && last_hash=`cat last.hash` || last_hash=none

# get current commit hash
current_hash=`git ls-remote http://github.com/mdijkstracpb/tsview.git refs/heads/master | cut -f 1`

if [ "$last_hash" = "$current_hash" ]; then
  # exit if no difference

  echo "************************************"
  echo "***                              ***"
  echo "***   STILL UP-TO-DATE           ***"
  echo "***                              ***"
  echo "************************************"

  exit 0;
fi

# update hash
echo $current_hash > last.hash

# remove all except hash and log dir
rm *.R* create-server-ui.sh

# install package
R -e 'devtools::install_github("mdijkstracpb/tsview")'

# get and run updated script that downloads R-files and create server and ui
wget https://raw.githubusercontent.com/mdijkstracpb/tsview/master/server/create-server-ui.sh
bash create-server-ui.sh

# reboot shiny server?
# NOT NEEDED: sudo reload shiny-server
