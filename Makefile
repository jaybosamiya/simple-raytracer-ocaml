all: SimpleRaytracer.native

%.native: %.ml
	corebuild $@

clean:
	rm -f *.native *~
