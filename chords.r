library(sound)

frq<-c(261.63,277.18,293.66,311.13,329.63,349.23,369.99,392,415.30,440,466.16,493.88)
noteNames<-c("C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B")
lookup<-data.frame(noteNames,frq)

scale<-function(x){ .5 + .5 * sin(pi * x*2 + pi/2) }
note<-function(x) { 12 * log(x/440)/log(2) + 49 }
idxFrq<-function(idx) { (idx - .5 ) }
magnitude<-function(x) { sqrt(Re(x) * Re(x) + Im(x) * Im(x)) }
closest<-function(x){ subset(lookup, select=c(noteNames,frq),subset=(min(noteDist(frq, x)) == noteDist(frq, x)))$noteNames }
noteDist<-function(note1, note2) { abs(log(note1)/log(2) - log(note2)/log(2)) %% 1 }

parseChord<-function(name, notes) {
    saveChord(name, notes)

    sample <- loadSample(paste("tests\\out\\",name,".wav"))


    fftdata<-fft(sample$sound)
    maxPitch<-length(fftdata)/2
    half<-fftdata[1:maxPitch]
    indexes<-c(1:maxPitch)
    magnitudes<-magnitude(half)

    scaleData<-scale(note(idxFrq(indexes)))
    scaledMagnitudes<-scaleData*magnitudes
    noteList<-mapply(closest, indexes-0.5)
    scaledMag<-data.frame(noteList, scaledMagnitudes)

    combined<-by(scaledMag$scaledMagnitudes, scaledMag$noteList, sum)
    foundNotes<-row.names(rev(sort(combined))[1:(length(notes))])
    print(paste("output-foundNotes", paste(foundNotes, collapse=","), ":"))
    print(paste("output-notes", paste(notes, collapse=",")))

    foundNotes<-sort(foundNotes)
    notes<-sort(notes)
    cmp<-identical(foundNotes, notes)
    
    if (!cmp)
    {
       print(
        paste(
          paste("foundNotes", paste(foundNotes, collapse=","), ":"),
          paste("notes", paste(notes, collapse=",")), collapse=":"))
    }

    checkTrue(cmp)
}

saveChord<-function(name, notes) {
   len<-0.9
   samples<-Sine(0, len)
   
   for(i in 1:length(notes)) {
     samples <- (samples + Sine(frq[match(notes[i], noteNames)], len))
   }

   saveSample(samples / (length(notes)) ,
              paste("tests\\out\\",name,".wav"), overwrite=TRUE)

   failures<-union(failures, c(name))
}
