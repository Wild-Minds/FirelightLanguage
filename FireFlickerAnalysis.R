# Fire Flicker Frequency Analysis
# Firelight paper PRSB Figure 2 section on flicker rates
# last updated: 25 Jun 2026


#if needed
#install.packages(c("tcltk", "openxlsx"))

suppressPackageStartupMessages({
  library(tcltk)     # GUI dialog
})

#rename data file to 'dat'
dat <- Flickerdata

# confirm structure
requiredCols <- c("Frame", "Red", "Green", "Blue", "Greyscale")
missingCols  <- setdiff(requiredCols, colnames(dat))
if (length(missingCols) > 0) {
  stop(sprintf("CSV is missing required column(s): %s",
               paste(missingCols, collapse = ", ")))
}

nFrames <- nrow(dat)
cat(sprintf("Loaded %d frames.\n", nFrames))

# name vectors
meanRedLevels    <- dat$Red
meanGreenLevels  <- dat$Green
meanBlueLevels   <- dat$Blue
meanGreyLevels   <- dat$Greyscale


#build Tk GUI dialog
inputDialog <- function(title, prompts, defaults) {
  tt   <- tktoplevel()
  tkwm.title(tt, title)
  vars <- lapply(defaults, tclVar)
  
  for (i in seq_along(prompts)) {
    tkgrid(tklabel(tt, text = prompts[i]),
           row = i - 1, column = 0, sticky = "w", padx = 8, pady = 4)
    tkgrid(tkentry(tt, textvariable = vars[[i]], width = 20),
           row = i - 1, column = 1, padx = 8, pady = 4)
  }
  
  result <- NULL
  onOK     <- function() { result <<- sapply(vars, tclvalue); tkdestroy(tt) }
  onCancel <- function() tkdestroy(tt)
  
  tkgrid(tkbutton(tt, text = "OK",     command = onOK),
         row = length(prompts), column = 0, pady = 10)
  tkgrid(tkbutton(tt, text = "Cancel", command = onCancel),
         row = length(prompts), column = 1, pady = 10)
  
  tkwait.window(tt)
  return(result)
}

userInput <- inputDialog(
  title    = "FFT Parameters",
  prompts  = c("Frame rate (frames per second):",
               "Maximum frequency to display (Hz):",
               "Channel to analyse (Red/Green/Blue/Greyscale):"),
  defaults = c("500", "10", "Red")
)

if (is.null(userInput)) stop("Input cancelled. Exiting.")

fs          <- as.numeric(userInput[1])   # Sampling frequency in Hz
userXAxis   <- as.numeric(userInput[2])   # Upper x-axis limit for FFT plot
channelName <- trimws(userInput[3])       # Channel label for plot titles

# Map channel choice to corresponding data vector.
channelMap <- list(
  Red       = meanRedLevels,
  Green     = meanGreenLevels,
  Blue      = meanBlueLevels,
  Greyscale = meanGreyLevels
)

# Case-insensitive matching.
matchIdx <- match(tolower(channelName), tolower(names(channelMap)))
if (is.na(matchIdx)) {
  warning(sprintf("Channel '%s' not recognised; defaulting to Red.", channelName))
  matchIdx    <- 1
  channelName <- "Red"
}
selectedChannel <- channelMap[[matchIdx]]

cat(sprintf("FFT channel: %s  |  fs: %.1f fps  |  display up to: %.1f Hz\n",
            channelName, fs, userXAxis))


# FFT calculation
# fft() returns the full complex DFT of length n.
# frequency axis spans 0 to fs in steps of fs/n.
# Power is |Y[k]|² / n  (normalised so power does not scale with length).

n     <- length(selectedChannel)
y     <- fft(selectedChannel)           # Complex DFT coefficients
f     <- (seq(0, n - 1)) * (fs / n)    # Frequency axis in Hz
power <- (Mod(y)^2) / n                # Normalised power spectrum



# Fig2b: FFT power spectrum
# x limited to [0, userXAxis], y ceiling set to
# second-highest peak (suppresses dominant DC component at f=0).

dev.new(width = 8, height = 5, noRStudioGD = TRUE)

# Logical index for display frequency window.
inRange <- f >= 0 & f <= userXAxis

# Find two largest power values in range; use second as the y ceiling.
# (NB DC component, f≈0, almost always the largest)

topTwo <- sort(power[inRange], decreasing = TRUE)[1:2]
yMax   <- topTwo[2]

plot(
  f[inRange], power[inRange],
  type  = "l",
  col   = "#5B9BD5",
  lwd   = 1.5,
  xlim  = c(0.15, userXAxis), #limit x axis to cut primary false peak at 0
  ylim  = c(0, yMax),
  xaxs = "i",
  yaxs = "i",
  xlab  = "Frequency (Hz)",
  ylab  = "Power",
  main  = sprintf("FFT of Mean%sLevels", channelName),
  cex.main = 1.4,
  cex.lab  = 1.1
)
#grid(col = "grey85", lty = 1)

# Report dominant non-DC peak frequency in console.
# Exclude lowest bin (f < 0.1 Hz) to capture slow drift vs real flicker
acRange  <- inRange & f > 0.1
peakFreq <- f[acRange][which.max(power[acRange])]
cat(sprintf("Dominant flicker frequency (excl. DC): %.2f Hz\n", peakFreq))



#Fig2a: Mean channel brightness over frames
#all four channels plotted against frame number

dev.new(width = 9, height = 5, noRStudioGD = TRUE)

frameIndex <- seq_len(nFrames)

# Calculate shared y range with small margin so no lines clipped.
allValues <- c(meanRedLevels, meanGreenLevels, meanBlueLevels, meanGreyLevels)
yLo <- floor(min(allValues))   - 0.5
yHi <- ceiling(max(allValues)) + 0.5

plot(
  frameIndex, meanRedLevels,
  type  = "l", col = "red", lwd = 1.2,
  xlim  = c(0, nFrames),
  ylim  = c(yLo, yHi),
  xaxs = "i",
  yaxs = "i",
  xlab  = "Frame Number",
  ylab  = "Mean R, G, B, and Grey Levels",
  main  = "Mean R, G, B, and Grey Levels",
  cex.main = 1.4,
  cex.lab  = 1.1
)
lines(frameIndex, meanGreenLevels,  col = "green3", lwd = 1.2)
lines(frameIndex, meanBlueLevels,   col = "blue",   lwd = 1.2)
lines(frameIndex, meanGreyLevels,   col = "black",  lwd = 1.8)
grid(col = "grey85", lty = 1)
legend(
  "topright",
  legend = c("Red", "Green", "Blue", "Grey"),
  col    = c("red", "green3", "blue", "black"),
  lwd    = c(1.2, 1.2, 1.2, 1.8),
  bty    = "n", cex = 0.85
)