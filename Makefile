.PHONY: all clean

FLAGS = -O2 -gnat2022
FONT = https://www.1001fonts.com/download/retron2000.zip
RAYLIB_LINUX = https://github.com/raysan5/raylib/releases/download/5.0/raylib-5.0_linux_amd64.tar.gz

all: build-linux 

Retron2000.ttf:
	wget $(FONT)
	unzip retron2000.zip -d .
	rm retron2000.zip

raylib-5.0_linux_amd64:
	wget $(RAYLIB_LINUX)
	tar -xvzf raylib-5.0_linux_amd64.tar.gz
	rm raylib-5.0_linux_amd64.tar.gz

build-linux: raylib-5.0_linux_amd64 Retron2000.ttf
	gnatmake $(FLAGS) -o palendar palendar.adb -Irgui \
		-largs -L./raylib-5.0_linux_amd64/lib/ -l:libraylib.a

clean:
	rm -rd raylib-*
	rm *.ali *.o *.ttf *.txt
	rm palendar
