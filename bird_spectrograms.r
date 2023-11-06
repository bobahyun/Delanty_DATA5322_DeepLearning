library(tidyverse)
library(tuneR)
library(signal)
library(zoo)
library(abind)

# function to create spectrograms 
# from https://hansenjohnson.org/post/spectrograms-in-r/
spec = function(snd, fs){
  nfft = 512
  window=256
  overlap=128
  
  spec = specgram(x = snd,
                  n = nfft,
                  Fs = fs,
                  window = window,
                  overlap = overlap
  )
  
  P = abs(spec$S)
  
  # normalize
  P = P/max(P)
  
  # plot spectrogram
  #image(t(P),useRaster=TRUE,axes=FALSE,col=grey(seq(1,0,length=180)))
  
  #return spectrogram in time x freq format
  t(P)
}


setwd("~/Documents/Curriculum/DATA5322-SQ23/Written Homeworks")

folderpath = '../Datasets/bird sounds/original_clips'
folders = list.dirs(folderpath)[-1] # remove main folder 

# loop through each species folder
for(h in 1:length(folders)){
  filepaths = list.files(folders[h], full.names = TRUE, pattern='\\.mp3')
  species = array(numeric(), c(1, 343, 256))
  
  #loop through all files within species
  for(f in 1:length(filepaths)){
    
    # read and normalize data
    fpath = filepaths[f]
    data = readMP3(fpath) %>% mono %>% downsample(22050)
    snd = data@left
    fs = data@samp.rate
    rm(data)
    
    # compute rolling average and loud segments
    d = data.frame(snd)%>% 
      mutate(avgamp = rollmean(abs(snd), k =fs/2, fill=0),
             isloud = avgamp>mean(avgamp)
             )
    
    loudid = which(d$isloud)
    rm(d)
    
    # detect start and end points of loud segments
    startp = c(loudid[1], loudid[which(diff(loudid)>1)+1])
    endp = c(loudid[which(diff(loudid)>1)], tail(loudid,1))

    # filter out short loud segments <0.5 and then get 2 second byte from each segment
    todrop = c()
    for(i in 1:length(startp)){
      if(endp[i]-startp[i]>=0.5*fs){
        endp[i] = startp[i]+2*fs
      } else {
        todrop = c(todrop,i)
      }
    }
    if(!is.null(todrop)){
      startp = startp[-todrop]
      endp = endp[-todrop]
    }

    # plot sound clip along with detected start and end times
    # plot(snd, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
    # plot(d$roll, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
    # abline(v=startp[1], col='red')
    # abline(v=endp[1], col='blue')
    
    # normalize sound
    snd = snd - mean(snd)
    
    # loop through individual sound clips in each file
    filespecs <- array(numeric(), c(length(startp), 343, 256))
    todrop = c()
    for(i in 1:length(startp)){
      out = spec(snd[startp[i]:endp[i]], fs)
      if(any(is.na(out))){
        print(paste0('na flag in ',fpath,', ',i))
        todrop = c(todrop,i)
      }else{
        filespecs[i,,] = out
      }
    }
    if(!is.null(todrop)){
      filespecs=filespecs[-todrop,,]
    }
    
  # bind file spectrograms with species spectrograms  
  species = abind(species, filespecs, along=1)
  rm(filespecs,snd)
  }
  # remove initialized spectrogram row
  species = species[-1,,]
  
  # save spectrorams as the species name
  sname = folders[h] %>% str_split('/') %>% unlist() %>% .[5]
  save(species, file=paste0('../Datasets/bird sounds/spectrograms2/',sname,'.dat'))
  rm(species)
}
