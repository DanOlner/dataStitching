from PyQt4.QtCore import *
from PyQt4.QtGui import *
from qgis.core import *
from qgis.gui import *
from qgis.analysis import *
import processing
import time
import os

def run_script(iface):

	#timer
	start = time.time()

	print(os.chdir('C:/Data'))

	#For tracking down QGIS processing tools by keyword search
	#print(processing.alglist('area'))

	#Target: single shapefile that other zones will intersect with (ideally larger than those going into it)
	#zones: dictionary of zones to intersect with target shapefile.
	#Dictionary key will be used for intersect output SHP filename
	def intersect(target, zones):

		for filename, zone in zones.iteritems():

			#Check it's a valid shapefile
			print(zone.isValid())

			#Intersect! And save...
			QgsOverlayAnalyzer().intersection(zone,target,"Census/Intersects/" + filename + ".shp",False)

			print('intersect ' + filename + ": " + str((time.time() - start)/60) + ' minutes')



	target = QgsVectorLayer(
		'MapPolygons/Scotland/1991/Scotland_postcodesectors_1991/scotland_pcs_1991.shp',
		# 'MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011/scotland_ig_2011.shp',
		# 'MapPolygons/Scotland/1991/Scotland_postcodesectors_1991/scotland_pcs_1991_uniqueIDsperRow_LochsRemoved_noSelfIntersect.shp',
		'target','ogr') 

	zones = {
	# 'Scotland_1971_EDs_to_1991_PCS': QgsVectorLayer(
	# 	'MapPolygons/Scotland/1971/Scotland_enumerationdistricts_1971_dissolvedZoneID/ed71dissolve.shp',
	# 	'1971','ogr'),
	# 'Scotland_1981_EDs_to_1991_PCS': QgsVectorLayer(
	# 	'MapPolygons/Scotland/1981/Scotland_enumerationdistricts_1981_dissolved_zoneIDs/ed81_dissolved_zoneIDs.shp',
	# 	'1981','ogr'),
	'Scotland_2001_OAs_to_2011_IZ': QgsVectorLayer(
	# 'Scotland_2001_OAs_to_1991_PCS_noSelfIntersect3': QgsVectorLayer(
		# 'MapPolygons/Scotland/2001/Scotland_outputareas_2001/scotland_oa_2001_dissolvedTo_OneIDperRow.shp',
		# 'MapPolygons/Scotland/2001/Scotland_outputareas_2001/scotland_oa_2001_dissolvedTo_OneIDperRow_noSelfIntersect.shp',
		'MapPolygons/Scotland/2001/Scotland_outputareas_2001/scotland_oa_2001.shp',
		'2001','ogr')
	# 'Scotland_2011_OAs_to_1991_PCS': QgsVectorLayer(
	# 	'MapPolygons/Scotland/2011/Scotland_output_areas_2011/scotland_oac_2011.shp',
	# 	'2011','ogr')
	}

	intersect(target,zones)    

	print('total time: ' + str((time.time() - start)/60) + ' minutes')