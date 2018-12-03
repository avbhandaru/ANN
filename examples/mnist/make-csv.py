#!/usr/bin/env python3
#code taken from https://stackoverflow.com/questions/49070242/converting-images-to-csv-file-in-python

from PIL import Image
import numpy as np
import sys
import os
import csv
import struct

#Useful function
def createFileList(myDir, format='.jpg'):
	fileList = []
	print(myDir)
	for root, dirs, files in os.walk(myDir, topdown=False):
	    for name in files:
	        if name.endswith(format):
	            fullName = os.path.join(root, name)
	            fileList.append(fullName)
	return fileList

def image_generator():
	# load the original image
	myFileList = createFileList('/Users/richardgreenbaum/desktop/dogs/n02108551-Tibetan_mastiff')

	for file in myFileList:
	    print(file)
	    img_file = Image.open(file)
	    # img_file.show()

	    # get original image parameters...
	    width, height = img_file.size
	    format = img_file.format
	    mode = img_file.mode

	    # Make image Greyscale
	    img_grey = img_file.convert('L')
	    #img_grey.save('result.png')
	    #img_grey.show()

	    # Save Greyscale values
	    value = np.asarray(img_grey.getdata(), dtype=np.float).reshape((img_grey.size[1], img_grey.size[0]))
	    value = value.flatten()
	    for x in range(len(value)):
	    	value[x] = value[x]/255
	    value = np.append(value, 2)
	    print(value[-1])
	    with open("dog_pixels.csv", 'a') as f:
	        writer = csv.writer(f)
	        writer.writerow(value)


def read_idx(filename):
    with open(filename, 'rb') as f:
        zero, data_type, dims = struct.unpack('>HBB', f.read(4))
        shape = tuple(struct.unpack('>I', f.read(4))[0] for d in range(dims))
        return np.fromstring(f.read(), dtype=np.uint8).reshape(shape)

image_array = read_idx('train-images-idx3-ubyte')
label_array = read_idx('train-labels-idx1-ubyte')
for count, image in enumerate(image_array):
    print("\r{:.4}".format(count/60000), end='')
    new_array = image.flatten() / 255
    new_array = np.append(new_array, label_array[count])
    with open("mnist.csv", 'a') as f:
        writer = csv.writer(f)
        writer.writerow(new_array)





