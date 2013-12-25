v4l2-webcam-frame-grabber
=========================

Webcam image grabber based on haskell v4l2-examples


A simple command line utility, grabs a video frame from a webcam peroidically, saves to a png file,
e.g. for monitoring purposes.


# v4l2-webcam-frame-grabber --help
The v4l2-webcam-frame-grabber program

v4l2-webcam-frame-grabber [OPTIONS]
  Grabs a video frame and saves it to a png file every delayseconds seconds

Common flags:
  -v --videodevice=ITEM  /dev/video0
  -o --outpng=ITEM       frame.png
  -d --delayseconds=INT  5 :: delay (in seconds) before grabbing next frame
     --verbose           run in a verbose mode
  -? --help              Display help message
  -V --version           Print version information

