DIRS = base gpib usb devcom instruments examples

all:
	-for dir in $(DIRS) ; do $(MAKE) -C $$dir $@ ; done

clean:
	-for dir in $(DIRS) ; do $(MAKE) -C $$dir $@ ; done
	rm -f *~
