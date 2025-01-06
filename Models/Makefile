$(shell mkdir -p tmp)

.PHONY: all clean
all: $(filter-out all, $(shell cat Makefile| sed -n '/\.glb:/p'| sed 's/:.\+//'))

clean:
	rm -rf tmp/

# Raiders Ion Array Frigate
tmp/Raiders\ Ion\ Array\ Frigate: src/Turanic\ Raiders/Raiders\ Ion\ Array\ Frigate.zip
	unzip -o "$<" -d tmp/

Raiders\ Ion\ Array\ Frigate.glb: tmp/Raiders\ Ion\ Array\ Frigate
	blender -b -P src/obj2glb.py -- "$</lod0/P1ionarrayfrigate.obj" "$@"

# Clee-San
tmp/Clee-San: src/Kiith\ Somtaaw/Clee-San.zip
	unzip -o "$<" -d "$@"

Decclesiam.glb: tmp/Clee-San
	blender -b -P src/obj2glb.py -- "$</lod0/Decclesiam.obj" "$@"

# Bentusi Mothership
tmp/Bentusi\ Mothership: src/Other/Bentusi\ Mothership.zip
	unzip -o "$<" -d tmp/

Bentusi\ Mothership.glb: tmp/Bentusi\ Mothership
	blender -b -P src/obj2glb.py -- "$</lod0/Floatingcity.obj" "$@"

# Taiidan Attack Bomber
tmp/Attack\ Bomber-T: src/Taiidan/./Taiidan\ Attack\ Bomber.zip
	unzip -o "$<" -d tmp/

Taiidan\ Attack\ Bomber.glb: tmp/Attack\ Bomber-T
	blender -b -P src/obj2glb.py -- "$</lod0/Attackbomber.obj" "$@"
