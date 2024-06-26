library(bookdown)


#remove the yml header
.copyModuleRmds <- SpaDES.docs::prepManualRmds("modules", rebuildCache = FALSE)
#giving me an error: 
# Error in file(con, "r") : cannot open the connection
# In addition: Warning message:
#  In file(con, "r") : cannot open file '2.Rmd': No such file or directory

#RENDERING
#this is currently rendering as an .Rmd and not html, not creating a docs folder. 
render_book(output_format = "all", envir = new.env())

# render_book(output_format = "bookdown::bs4_book", envir = new.env())


render_book(output_format = 'all', output_dir = 'docs', config_file = '_bookdown.yml')
render_book(output_format = "bookdown::bs4_book", envir = new.env())


