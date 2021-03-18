library("tuneR")
library("seewave")
library(signal, warn.conflicts = F, quietly = T)
library(oce, warn.conflicts = F, quietly = T) 


glos <- readWave("sample.wav")
klasniecie <- readWave("klasniecie.wav")

filter <- function(wave){
  wave <- ffilter(wave, from=0, to=280)
  wave
}


duration <- function(sample){
  dur = length(sample@left)/sample@samp.rate
  dur 
}

sample_rate <- function(sample){
  fs = sample@samp.rate
  fs 
}

waveform <- function(sample){
  sl <- sample@left
  sl = sl - mean(sl)
  plot(sl, type = 'l', xlab = 'Samples', ylab = 'Amplitude')
}

spectogram <- function(sample){
  sl <- sample@left
  spec = specgram(x = sl,
                  n = 1024,
                  Fs = sample_rate(sample),
                  window = 256,
                  overlap = 128)
  
  P = abs(spec$S)
  P = P/max(P)
  P = 10*log10(P)
  t = spec$t
  
  imagep(x = t, y = spec$f, z = t(P), col = oce.colorsViridis, ylab = 'Frequency [Hz]', xlab = 'Time [s]', drawPalette = T, decimate = F)
}



filtered_glos <- filter(glos)
filtered_klasniecie <- filter(klasniecie)


duration(glos)
sample_rate(glos)
waveform(glos)
spectogram(glos)

#Funkcja do voice na dataset
specprop(meanspec(filtered_glos, f= 44100), f=44100)
specprop(meanspec(filtered_klasniecie, f= 44100), f=44100)



