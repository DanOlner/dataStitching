from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from qgis.gui import *
from qgis.analysis import *
import processing
import time
import os

def run_script(iface):

	#time whole thing
	start = time.time()

	print(os.chdir('C:/Data'))

	#print(processing.alglist('area'))

	


	#~~~~~~~~~~~~~~~~~
	#81 EDs into 2011 IGs
	#Using the '81 dissolve' file where zones on multiple rows have been turned into
	#single-row multiparts
	eds81d = QgsVectorLayer(
		'MapPolygons/Scotland/1981/Scotland_enumerationdistricts_1981_dissolved_zoneIDs/ed81_dissolved_zoneIDs.shp',
		'ed81dissolved','ogr')
	print(eds81d.isValid())
	
	#intermediate zones 2011
	ims = QgsVectorLayer(
		'MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011/scotland_ig_2011.shp',
		'izs11','ogr')
	print(ims.isValid())

	#QgsMapLayerRegistry.instance().addMapLayers([cas,ims])

	#Intersect test
	#http://gis.stackexchange.com/questions/54881/how-to-use-qgsoverlayanalyzer-class-in-pyqgis
	#Smaller first, into larger.
	QgsOverlayAnalyzer().intersection(eds81d,ims,"MapPolygons/Intersections/Scotland/1981_dissolved_EDsTo2011_IGs/1981_dissolved_EDsTo2011_IGs.shp",False)

	# rez = QgsVectorLayer(
	# 	'MapPolygons/Intersections/Scotland/1971_EDsTo2011_IGs/1971_EDsTo2011_IGs.shp',
	# 	'intersectResult','ogr')
	# print(rez.isValid())

	#QgsMapLayerRegistry.instance().addMapLayers([rez])

	print('total time: ' + str((time.time() - start)/60) + ' minutes')



	
	