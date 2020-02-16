# author Nils von Norsinski
# apply a series of box or gaussian filters with varien size of moving window

import cv2
import imageio
from matplotlib import pyplot as plt



#img=cv2.imread("dgm_1m_AOI_100.tif",cv2.IMREAD_LOAD_GDAL | cv2.IMREAD_ANYDEPTH)
img=cv2.imread("dgm_1m_AOI_100.tif",cv2.IMREAD_LOAD_GDAL | cv2.IMREAD_ANYDEPTH)

# radius of filter
li = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 35, 40, 45, 50]
# for naming
li2 = [101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122]

# actual filter size
for i in range(len(li)):
    li[i] = li[i]*2+1

filename = 'D:/Nils-Laptop/Documents/Masterarbeit/R-Code/Functional_data_Landslides/Daten/Paldau/Parameters/filtered_images_dgm/dgm_1m_AOI_'

for i in range(len(li)):
   print(i)
   #dst = cv2.blur(img, (li[i], li[i]))
   dst = cv2.GaussianBlur(img, (li[i], li[i]), 0)
   file = filename + str(li2[i]) + '.tif'
   imageio.imwrite(file, dst)


plt.figure('Bilder')
plt.subplot(121)
plt.imshow(img, cmap='gray')

plt.subplot(122)
plt.imshow(dst, cmap='gray')

plt.show()
