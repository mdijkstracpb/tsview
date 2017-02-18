# Download R files from github to create server/ui

base_url=https://raw.githubusercontent.com/mdijkstracpb/tsview/master/R/

R_main_files="server.R
ui.R"

R_help_files="ext-switch-button.R
tsview-helper.R
tsview-plot.R
tsview-settings.R
tsview-tsplot.R
tsview-tsview.R"

# download server and ui
for f in ${R_main_files[@]}
do
	wget -O ${f}.original ${base_url}$f
done

# download help files
for f in ${R_help_files[@]}
do
	wget ${base_url}$f
done

# make source file for server and ui
> source-these-files.R.help
for each in *.R
do
  echo "source(\"$each\")" >> source-these-files.R.help
done

# compose server and ui
for f in ${R_main_files[@]}
do
  cat source-these-files.R.help ${f}.original > ${f}
done

# Give our 'wrapped server' the shiny server
echo "shiny::shinyServer(.wrapped_server(NULL))" >> server.R
