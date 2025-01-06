import bpy
import os
import glob

from math import radians

# Clear scene
context = bpy.context
scene = context.scene

# Cleanup
for c in bpy.data.objects:
	bpy.data.objects.remove(c)
for collection in bpy.data.collections:
	bpy.data.collections.remove(collection)

# Get arguments
import sys
argv = sys.argv
argv = argv[argv.index("--") + 1:]  # get all args after "--"

# Import OBJ
bpy.ops.import_scene.obj(filepath=argv[0])

# Rotate model to the proper rotation
#  -Z is forward, Y is up, X is right
model = bpy.context.scene.objects[0]
for v in model.data.vertices:
    x, y, z = v.co
    v.co = (-y, z, x)

# reexport GLTF
bpy.ops.export_scene.gltf(filepath=argv[1]
	,export_texcoords=True
	,export_normals=True
	,export_colors=True
	,export_cameras=False, export_lights=False)
