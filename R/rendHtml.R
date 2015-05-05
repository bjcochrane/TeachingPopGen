# converts .Rmd file to .html and stores in a folder called html
rendHtml <-function(file,out.dir="html"){
  f.name <-paste(file,".Rmd",sep="")
  rmarkdown::render(f.name,output_dir=paste('./',out.dir,sep=""))
}