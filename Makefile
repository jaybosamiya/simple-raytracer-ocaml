all: SimpleRaytracer.native

%.native: %.ml
	corebuild $@
