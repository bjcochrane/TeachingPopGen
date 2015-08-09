# converts .Rmd file to .html and stores in a folder called html
rendHtml <-function(file,out.dir="html"){
  f.name <-paste(file,"Rmd",sep=".")
  outfile <-paste(file,"html",sep=".")
  rmarkdown::render(f.name,output_file=outfile,output_dir=paste('./',out.dir,sep=""))
}