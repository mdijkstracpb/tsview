# auto deploy server

# get last commit hash
[ -f last.hash ] && last_hash=`echo last.hash` || last_hash=none

# get current commit hash
current_hash=`git ls-remote http://github.com/mdijkstracpb/tsview.git refs/heads/master | cut -f 1`

# exit if no difference
[[ last_hash -eq current_hash ]] && exit 0

# install package
R -e 'devtools::install_github("mdijkstracpb/tsview")'

# get and run updated script that downloads R-files and create server and ui
wget https://raw.githubusercontent.com/mdijkstracpb/tsview/master/server/create-server-ui.sh
bash create-server-ui.sh

# reboot shiny server?
sudo reload shiny-server