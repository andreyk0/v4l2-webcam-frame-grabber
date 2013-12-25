
build:
	cabal build

test:
	./dist/build/v4l2-webcam-frame-grabber/v4l2-webcam-frame-grabber --verbose -v /dev/video0

clean:
	cabal clean

deps: cabal.sandbox.config
	cabal install --only-dependencies --enable-library-profiling

cabal.sandbox.config:
	cabal sandbox init
